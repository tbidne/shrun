{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad ((>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.Reader qualified as MTL
import Data.Maybe qualified as May
import ShellRun.Class.MonadLogger (LogLevel (..), LogMode (..), MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.IO qualified as ShIO
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Env (Env (..), NativeLog (..))
import ShellRun.Types.IO (Stderr (..))
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Utils qualified as U
import System.Clock (TimeSpec)
import System.Clock qualified as C
import UnliftIO.Async qualified as UAsync

runCommands :: (MonadIO m, MonadLogger m, MonadUnliftIO m, MonadReader Env m) => [Command] -> m ()
runCommands commands = do
  start <- MTL.liftIO $ C.getTime C.Monotonic
  actionsAsync <- UAsync.async $ UAsync.mapConcurrently_ runCommand commands

  counter actionsAsync
  end <- MTL.liftIO $ C.getTime C.Monotonic
  let totalTime = U.diffTime start end
  ML.clearLine
  ML.logInfoBlue "Finished!"
  ML.logInfoBlue $ "Total time elapsed: " <> U.formatSeconds totalTime

runCommand :: (MonadReader Env m, MonadIO m) => Command -> m ()
runCommand command@(MkCommand cmd) = do
  nativeLog <- MTL.asks nativeLog
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
    logFn $ "Time elapsed: " <> U.formatSeconds seconds <> "\n"

counter :: (MonadReader Env m, MonadIO m) => Async a -> m ()
counter asyn = do
  timeout <- MTL.asks timeout
  MTL.liftIO $ do
    start <- C.getTime C.Monotonic
    Loops.whileM_ (keepRunning asyn start timeout) $ do
      ShIO.sh_ "sleep 1" Nothing
      -- TODO: We can just count, don't need IO action probably
      elapsed <- C.getTime C.Monotonic
      let diff = U.diffTime start elapsed
      ML.resetCR
      ML.logLevelMode InfoCyan NoLine $ "Running time: " <> U.formatSeconds diff

keepRunning :: Async a -> TimeSpec -> Maybe NonNegative -> IO Bool
keepRunning asyn start to = do
  running <- unfinished asyn
  currTime <- C.getTime C.Monotonic
  let hasTimedOut = timedOut start currTime to
  if running && hasTimedOut
    then do
      Async.cancel asyn
      ML.clearLine
      ML.logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure running

timedOut :: TimeSpec -> TimeSpec -> Maybe NonNegative -> Bool
timedOut start curr =
  \case
    Nothing -> False
    Just t ->
      let timeSoFar = U.diffTime start curr
       in timeSoFar > t

unfinished :: Async a -> IO Bool
unfinished = Async.poll >=> pure . May.isNothing