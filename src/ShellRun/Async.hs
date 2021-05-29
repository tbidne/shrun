{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad ((>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.Reader qualified as MTL
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Maybe qualified as May
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
  actionsAsync <- UAsync.async $ UAsync.mapConcurrently_ runCommand commands

  counter actionsAsync
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

counter :: (HasTimeout env, MonadReader env m, MonadIO m) => Async a -> m ()
counter asyn = do
  timeout <- MTL.asks getTimeout
  MTL.liftIO $ do
    timer <- IORef.newIORef $ NN.unsafeNonNegative 0
    let inc = NN.unsafeNonNegative 1
    Loops.whileM_ (keepRunning asyn timer timeout) $ do
      Concurrent.threadDelay 1_000_000
      IORef.modifyIORef' timer (+:+ inc)
      elapsed <- IORef.readIORef timer
      ML.resetCR
      ML.logLevelMode InfoCyan NoLine $ "Running time: " <> U.formatTime elapsed

keepRunning :: Async a -> IORef NonNegative -> Maybe NonNegative -> IO Bool
keepRunning asyn timer to = do
  running <- unfinished asyn
  elapsed <- IORef.readIORef timer
  let hasTimedOut = timedOut elapsed to
  if running && hasTimedOut
    then do
      Async.cancel asyn
      ML.clearLine
      ML.logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure running

timedOut :: NonNegative -> Maybe NonNegative -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just t -> timer > t

unfinished :: Async a -> IO Bool
unfinished = Async.poll >=> pure . May.isNothing
