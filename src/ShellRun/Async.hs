{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception (IOException)
import Control.Exception qualified as Except
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.Reader qualified as MTL
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import ShellRun.Class.Has (HasNativeLog (..), HasTimeout (..))
import ShellRun.Class.MonadLogger (LogLevel (..), LogMode (..), MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.IO qualified as ShIO
import ShellRun.Math (RAdd (..))
import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Env (NativeLog (..))
import ShellRun.Types.IO (Stderr (..))
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import UnliftIO qualified
import UnliftIO.Async qualified as UAsync

runCommands ::
  ( HasNativeLog env,
    HasTimeout env,
    MonadIO m,
    MonadLogger m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  [Command] ->
  m ()
runCommands commands = do
  start <- MTL.liftIO $ C.getTime Monotonic
  let actions = UAsync.mapConcurrently_ runCommand commands
  let actionsWithTimer = UAsync.race_ actions counter

  result :: Either IOException () <- UnliftIO.withRunInIO $ \runner -> Except.try $ runner actionsWithTimer

  case result of
    Left ex ->
      ML.logFatal $
        T.pack $
          "Encountered an exception. This is likely not an error in any of the "
            <> "commands run, but rather an error in ShellRun itself: "
            <> Except.displayException ex
    Right _ -> pure ()

  end <- MTL.liftIO $ C.getTime Monotonic
  let totalTime = U.diffTime start end
  ML.clearLine
  ML.logInfoBlue "Finished!"
  ML.logInfoBlue $ "Total time elapsed: " <> U.formatTime totalTime

runCommand ::
  ( HasNativeLog env,
    MonadReader env m,
    MonadIO m
  ) =>
  Command ->
  m ()
runCommand command@(MkCommand cmd) = do
  nativeLog <- MTL.asks getNativeLog
  let shFn = case nativeLog of
        None -> ShIO.tryTimeSh
        Stdout -> ShIO.tryTimeShWithStdout

  MTL.liftIO $ do
    res <- shFn command Nothing
    (seconds, logFn, msg) <- case res of
      Left (t, MkStderr err) -> pure (t, ML.logError, err)
      Right t -> pure (t, ML.logInfoSuccess, "Successfully ran `" <> cmd <> "`")
    ML.clearLine
    logFn msg
    logFn $ "Time elapsed: " <> U.formatTime seconds <> "\n"

counter :: (HasTimeout env, MonadReader env m, MonadIO m) => m ()
counter = do
  timeout <- MTL.asks getTimeout
  MTL.liftIO $ do
    timer <- IORef.newIORef $ NN.unsafeNonNegative 0
    let inc = NN.unsafeNonNegative 1
    Loops.whileM_ (keepRunning timer timeout) $ do
      Concurrent.threadDelay 1_000_000
      IORef.modifyIORef' timer (+:+ inc)
      elapsed <- IORef.readIORef timer
      ML.resetCR
      ML.logLevelMode InfoCyan NoLine $ "Running time: " <> U.formatTime elapsed

keepRunning :: IORef NonNegative -> Maybe NonNegative -> IO Bool
keepRunning timer to = do
  elapsed <- IORef.readIORef timer
  if timedOut elapsed to
    then do
      ML.clearLine
      ML.logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure True

timedOut :: NonNegative -> Maybe NonNegative -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just t -> timer > t