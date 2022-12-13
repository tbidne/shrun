-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
--
-- @since 0.1
module Shrun
  ( ShellT,
    runShellT,
    shrun,
  )
where

import Control.Monad.Loops qualified as Loops
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time.Relative (formatRelativeTime, formatSeconds)
import Effects.MonadSTM (MonadTBQueue (tryReadTBQueueM))
import Effects.MonadThread as X (MonadThread (microsleep), sleep)
import Effects.MonadTime (MonadTime (..), TimeSpec, withTiming)
import Shrun.Configuration.Env.Types
  ( FileLogging,
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Data.Timeout (Timeout (..))
import Shrun.IO (Stderr (..), tryCommandLogging)
import Shrun.Logging.Formatting qualified as LFormat
import Shrun.Logging.Log qualified as Log
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.Logging.Types
  ( FileLog,
    Log (..),
    LogLevel (..),
    LogMode (..),
    LogRegion (..),
  )
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import Shrun.Utils qualified as Utils
import System.Console.Regions (RegionLayout (..))
import System.Console.Regions qualified as Regions
import UnliftIO.Async qualified as Async

-- | Entry point
--
-- @since 0.1
shrun ::
  ( HasCommands env,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadCallStack m,
    MonadIORef m,
    MonadFsWriter m,
    MonadMask m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m,
    RegionLogger m
  ) =>
  m ()
shrun = asks getCommands >>= initLogging

-- | Initializes the loggers and runs the commands.
initLogging ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadCallStack m,
    MonadFsWriter m,
    MonadIORef m,
    MonadMask m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m,
    RegionLogger m
  ) =>
  NonEmptySeq Command ->
  m ()
initLogging commands = Regions.displayConsoleRegions $ do
  logging <- asks getLogging

  -- always start console logger
  Async.withAsync pollQueueToConsole $ \consoleLogger -> do
    -- run commands, running file logger if requested
    maybe
      runCommands
      runWithFileLogging
      (logging ^. #fileLogging)

    -- cancel consoleLogger, print remaining logs
    Async.cancel consoleLogger
    let consoleQueue = logging ^. #consoleLogging
    flushTBQueueM consoleQueue >>= traverse_ printConsoleLog
  where
    runWithFileLogging fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueM queue >>= traverse_ (logFile h)
        hFlush h
      where
        (h, queue) = fileLogging ^. #log

    runCommands = do
      (totalTime, result) <- withTiming $ tryAny actionsWithTimer
      printFinalResult totalTime result
      where
        actions = Async.mapConcurrently_ runCommand commands
        actionsWithTimer = Async.race_ actions (counter commands)

runCommand ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadMask m,
    MonadCallStack m,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  Command ->
  m ()
runCommand cmd = do
  cmdResult <- tryCommandLogging cmd

  Regions.withConsoleRegion Linear $ \r -> do
    let (msg', lvl', t') = case cmdResult of
          Left (t, MkStderr err) -> (": " <> err, LevelError, t)
          Right t -> ("", LevelSuccess, t)
    Log.putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = T.pack (formatRelativeTime t') <> msg',
          lvl = lvl',
          mode = LogModeFinish
        }

printFinalResult ::
  ( Exception e,
    HasLogging env,
    MonadIO m,
    MonadReader env m,
    MonadMask m,
    MonadTBQueue m,
    MonadTime m
  ) =>
  TimeSpec ->
  Either e b ->
  m ()
printFinalResult totalTime result = Regions.withConsoleRegion Linear $ \r -> do
  case result of
    Left ex -> do
      let errMsg =
            T.pack $
              "Encountered an exception. This is likely not an error in any of the "
                <> "commands run but rather an error in Shrun itself: "
                <> displayException ex
          fatalLog =
            MkLog
              { cmd = Nothing,
                msg = errMsg,
                lvl = LevelFatal,
                mode = LogModeFinish
              }
      Log.putRegionLog r fatalLog
    Right _ -> pure ()

  let totalTimeTxt = formatRelativeTime (Utils.timeSpecToRelTime totalTime)
      finalLog =
        MkLog
          { cmd = Nothing,
            msg = T.pack totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  Log.putRegionLog r finalLog

counter ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTVar m,
    RegionLogger m,
    MonadTime m
  ) =>
  NonEmptySeq Command ->
  m ()
counter cmds = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  microsleep 100_000
  withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- newIORef 0
    Loops.whileM_ (keepRunning cmds r timer timeout) $ do
      elapsed <- do
        sleep 1
        modifyIORef' timer (+ 1)
        readIORef timer
      logCounter r elapsed

logCounter ::
  ( HasLogging env,
    MonadReader env m,
    MonadTBQueue m
  ) =>
  ConsoleRegion ->
  Natural ->
  m ()
logCounter region elapsed = do
  logging <- asks getLogging
  let lg =
        MkLog
          { cmd = Nothing,
            msg = T.pack (formatSeconds elapsed),
            lvl = LevelTimer,
            mode = LogModeSet
          }
  Log.regionLogToConsoleQueue region logging lg

keepRunning ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m
  ) =>
  NonEmptySeq Command ->
  ConsoleRegion ->
  IORef Natural ->
  Maybe Timeout ->
  m Bool
keepRunning allCmds region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      cmdDisplay <- asks (view #cmdDisplay . getLogging)
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- readTVarM completedCmdsTVar

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ NESeq.toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LFormat.displayCmd cmd cmdDisplay : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Log.putRegionLog region $
        MkLog
          { cmd = Nothing,
            msg = "Timed out, cancelling remaining commands: " <> unfinishedCmds,
            lvl = LevelWarn,
            mode = LogModeFinish
          }
      pure False
    else pure True

timedOut :: Natural -> Maybe Timeout -> Bool
timedOut _ Nothing = False
timedOut timer (Just (MkTimeout t)) = timer > t

pollQueueToConsole ::
  ( HasLogging env,
    MonadFsWriter m,
    MonadReader env m,
    MonadTBQueue m,
    RegionLogger m
  ) =>
  m void
pollQueueToConsole = do
  queue <- asks (view #consoleLogging . getLogging)
  forever $ tryReadTBQueueM queue >>= traverse_ printConsoleLog

printConsoleLog :: RegionLogger m => LogRegion -> m ()
printConsoleLog (LogNoRegion consoleLog) = logFn (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logModeToRegionFn m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  ( MonadFsWriter m,
    MonadTBQueue m
  ) =>
  FileLogging ->
  m void
pollQueueToFile fileLogging = forever $ tryReadTBQueueM queue >>= traverse_ (logFile h)
  where
    (h, queue) = fileLogging ^. #log

logFile :: MonadFsWriter m => Handle -> FileLog -> m ()
logFile h = hPutUtf8 h . view #unFileLog
