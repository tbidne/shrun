module ShellRunner
  ( runCommands,
  )
where

import Control.Concurrent.Async (Async (..))
import Control.Concurrent.Async qualified as A
import Control.Monad ((>=>))
import Control.Monad.Loops qualified as Loops
import Data.Maybe qualified as May
import ShellRunner.IO qualified as ShIO
import ShellRunner.Logger qualified as L
import ShellRunner.Types.Command (Command (..))
import ShellRunner.Types.IO (Stderr (..))
import ShellRunner.Types.NonNegative (NonNegative)
import ShellRunner.Utils qualified as U
import System.Clock (TimeSpec)
import System.Clock qualified as C

runCommands :: [Command] -> Maybe NonNegative -> IO ()
runCommands commands timeout = do
  start <- C.getTime C.Monotonic
  actionAsync <- A.async $ A.mapConcurrently_ runCommand commands
  counter actionAsync timeout
  end <- C.getTime C.Monotonic
  let totalTime = U.diffTime start end
  L.clearLine
  L.logInfoBlue "Finished!"
  L.logInfoBlue $ "Total time elapsed: " <> U.formatSeconds totalTime

runCommand :: Command -> IO ()
runCommand command@(MkCommand cmd) = do
  res <- ShIO.tryTimeSh command Nothing
  (seconds, logFn, msg) <- case res of
    Left (t, MkStderr err) -> pure (t, L.logError, err)
    Right (t, _) -> pure (t, L.logInfoSuccess, "Successfully ran `" <> cmd <> "`")
  L.clearLine
  logFn msg
  logFn $ "Time elapsed: " <> U.formatSeconds seconds <> "\n"

counter :: Async a -> Maybe NonNegative -> IO ()
counter asyn timeout = do
  start <- C.getTime C.Monotonic
  Loops.whileM_ (keepRunning asyn start timeout) $ do
    ShIO.sh_ "sleep 1" Nothing
    -- TODO: We can just count, don't need IO action probably
    elapsed <- C.getTime C.Monotonic
    let diff = U.diffTime start elapsed
    L.resetCR
    L.logNoLine $ "Running time: " <> U.formatSeconds diff

keepRunning :: Async a -> TimeSpec -> Maybe NonNegative -> IO Bool
keepRunning asyn start to = do
  running <- unfinished asyn
  currTime <- C.getTime C.Monotonic
  let hasTimedOut = timedOut start currTime to
  if running && hasTimedOut
    then do
      A.cancel asyn
      L.clearLine
      L.logWarn "Timed out, cancelling remaining tasks."
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
unfinished = A.poll >=> pure . May.isNothing
