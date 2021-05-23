{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Class.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent.Async (Async (..))
import Control.Concurrent.Async qualified as A
import Control.Monad ((>=>))
import Control.Monad.Loops qualified as Loops
import Data.Maybe qualified as May
import Data.Text (Text)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.IO qualified as ShIO
import ShellRun.Parsing.Args qualified as ParseArgs
import ShellRun.Parsing.Legend qualified as ParseLegend
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.IO (Stderr (..))
import ShellRun.Types.Legend (LegendErr, LegendMap)
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Utils qualified as U
import System.Clock (TimeSpec)
import System.Clock qualified as C

class Monad m => MonadShell m where
  parseArgs :: m Args
  legendPathToMap :: Text -> m (Either LegendErr LegendMap)
  runCommands :: [Command] -> Maybe NonNegative -> m ()

instance MonadShell IO where
  parseArgs :: IO Args
  parseArgs = ParseArgs.runParser

  legendPathToMap :: Text -> IO (Either LegendErr LegendMap)
  legendPathToMap = ParseLegend.legendPathToMap

  runCommands :: [Command] -> Maybe NonNegative -> IO ()
  runCommands commands timeout = do
    start <- C.getTime C.Monotonic
    actionAsync <- A.async $ A.mapConcurrently_ runCommand commands
    counter actionAsync timeout
    end <- C.getTime C.Monotonic
    let totalTime = U.diffTime start end
    ML.clearLine
    ML.logInfoBlue "Finished!"
    ML.logInfoBlue $ "Total time elapsed: " <> U.formatSeconds totalTime

runCommand :: Command -> IO ()
runCommand command@(MkCommand cmd) = do
  res <- ShIO.tryTimeShWithStdout command Nothing
  (seconds, logFn, msg) <- case res of
    Left (t, MkStderr err) -> pure (t, ML.logError, err)
    Right t -> pure (t, ML.logInfoSuccess, "Successfully ran `" <> cmd <> "`")
  ML.clearLine
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
    ML.resetCR
    ML.logNoLineCyan $ "Running time: " <> U.formatSeconds diff

keepRunning :: Async a -> TimeSpec -> Maybe NonNegative -> IO Bool
keepRunning asyn start to = do
  running <- unfinished asyn
  currTime <- C.getTime C.Monotonic
  let hasTimedOut = timedOut start currTime to
  if running && hasTimedOut
    then do
      A.cancel asyn
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
unfinished = A.poll >=> pure . May.isNothing