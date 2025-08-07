-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( ShellT,
    runShellT,
    shrun,
  )
where

import Control.Exception.Utils qualified as Ex.Utils
import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.HashSet qualified as Set
import Effects.Concurrent.Async qualified as Async
import Effects.Time (TimeSpec, withTiming)
import Shrun.Configuration.Data.CommonLogging (CommonLoggingEnv)
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat qualified as TimerFormat
import Shrun.Configuration.Data.Core.Timeout (Timeout (MkTimeout))
import Shrun.Configuration.Data.FileLogging
  ( FileLogOpened (MkFileLogOpened),
    FileLoggingEnv,
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction
      ( NotifyAll,
        NotifyCommand,
        NotifyFinal
      ),
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError (getAnyError),
    HasCommandLogging,
    HasCommands (getCommands, getCompletedCommands),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit,
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO
  ( CommandResult (CommandFailure, CommandSuccess),
    Stderr (MkStderr),
    tryCommandLogging,
  )
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Formatting qualified as LogFmt
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        displayRegions,
        logGlobal,
        logRegion,
        withRegion
      ),
  )
import Shrun.Logging.Types
  ( ConsoleLog,
    FileLog,
    Log (MkLog, cmd, lvl, mode, msg),
    LogLevel
      ( LevelError,
        LevelFatal,
        LevelFinished,
        LevelSuccess,
        LevelTimer,
        LevelWarn
      ),
    LogMode (LogModeFinish, LogModeSet),
    LogRegion (LogNoRegion, LogRegion),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Notify qualified as Notify
import Shrun.Notify.MonadNotify (MonadNotify, NotifyMessage)
import Shrun.Notify.MonadNotify qualified as MonadNotify
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import Shrun.Utils qualified as Utils

-- | Entry point
shrun ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasNotifyConfig env,
    HasTimeout env,
    MonadAsync m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadNotify m,
    MonadTypedProcess m,
    MonadMask m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  -- | .
  m ()
shrun = displayRegions $ flip Ex.Utils.onAsyncException teardown $ do
  mFileLogging <- asks getFileLogging
  (_, consoleQueue) <- asks getConsoleLogging

  -- always start console logger
  Async.withAsync (pollQueueToConsole consoleQueue) $ \consoleLogger -> do
    -- run commands, running file logger if requested
    maybe
      runCommands
      runWithFileLogging
      mFileLogging

    -- cancel consoleLogger, print remaining logs
    Async.cancel consoleLogger
    flushTBQueueA consoleQueue >>= traverse_ printConsoleLog

    -- if any processes have failed, exit with an error
    anyError <- readTVarA =<< asks getAnyError
    when anyError exitFailure
  where
    runWithFileLogging :: (HasCallStack) => FileLoggingEnv -> m ()
    runWithFileLogging fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueA fileQueue >>= traverse_ (logFile h)
        hFlush h
      where
        MkFileLogOpened h fileQueue = fileLogging ^. #file

    runCommands :: (HasCallStack) => m ()
    runCommands = do
      cmds <- asks getCommands
      let actions = Async.mapConcurrently_ runCommand cmds
          actionsWithTimer = Async.race_ actions counter

      (totalTime, result) <- withTiming $ trySync actionsWithTimer
      printFinalResult totalTime result
{-# INLINEABLE shrun #-}

runCommand ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasNotifyConfig env,
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadNotify m,
    MonadTypedProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  CommandP1 ->
  m ()
runCommand cmd = do
  cmdResult <- tryCommandLogging cmd
  commonLogging <- asks getCommonLogging
  (consoleLogging, consoleQueue) <- asks (getConsoleLogging @env @(Region m))

  let (urgency, consoleLog, mkFileLog, notifyMsg) =
        mkResultData commonLogging consoleLogging cmd cmdResult

  putCommandFinalLog consoleQueue consoleLog mkFileLog

  let commandNameTrunc = consoleLogging ^. #commandNameTrunc
      keyHide = commonLogging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide commandNameTrunc cmd

  -- Sent off notif if NotifyAll or NotifyCommand is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll ->
      Notify.sendNotif (MonadNotify.fromUnlined $ formattedCmd <> " Finished") notifyMsg urgency
    Just NotifyCommand ->
      Notify.sendNotif (MonadNotify.fromUnlined $ formattedCmd <> " Finished") notifyMsg urgency
    _ -> pure ()
{-# INLINEABLE runCommand #-}

-- | Prints the final log from the command (i.e. success/error message).
-- Has different log depending on the output (i.e. if we should log
-- multiple lines).
putCommandFinalLog ::
  forall m env.
  ( HasCallStack,
    HasFileLogging env,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  TBQueue (LogRegion (Region m)) ->
  ConsoleLog ->
  (FileLoggingEnv -> m FileLog) ->
  m ()
putCommandFinalLog consoleQueue consoleLog mkFileLog = do
  withRegion Linear $ \r -> writeTBQueueA consoleQueue (LogRegion mode r consoleLog)

  mFileLogging <- asks getFileLogging
  for_ mFileLogging $ \fl -> do
    fileLog <- mkFileLog fl
    Logging.logToFileQueue fl fileLog
  where
    mode = LogModeFinish
{-# INLINEABLE putCommandFinalLog #-}

-- | All of the command result data needed for final log.
type CommandResultData m =
  Tuple4
    -- Urgency level for notifs
    UrgencyLevel
    -- Console log
    ConsoleLog
    -- File log, if active
    (FileLoggingEnv -> m FileLog)
    -- Notif body
    NotifyMessage

-- | Gets log data from CommandResult.
mkResultData ::
  forall m.
  (MonadTime m) =>
  CommonLoggingEnv ->
  ConsoleLoggingEnv ->
  CommandP1 ->
  CommandResult ->
  CommandResultData m
mkResultData commonLogging consoleLogging cmd cmdResult =
  (urgency, consoleLog, mMkFileLog, notifyMsg)
  where
    timerFormat = consoleLogging ^. #timerFormat
    keyHide = commonLogging ^. #keyHide

    (urgency, lvl, rt, messages) = case cmdResult of
      CommandFailure t (MkStderr []) -> (Critical, LevelError, t, ["<no error message>"])
      CommandFailure t (MkStderr errs) -> (Critical, LevelError, t, errs)
      CommandSuccess t -> (Normal, LevelSuccess, t, [])

    timeMsg = TimerFormat.formatRelativeTime timerFormat rt
    notifyMsg = Notify.formatNotifyMessage timeMsg messages

    (consoleLog, mMkFileLog) = case messages of
      -- 1. No message (success). Just print out the time.
      [] ->
        let log =
              MkLog
                { cmd = Just cmd,
                  msg = Types.fromUnlined timeMsg,
                  lvl,
                  mode
                }
         in ( Formatting.formatConsoleLog keyHide consoleLogging log,
              \fl -> Formatting.formatFileLog keyHide fl log
            )
      -- 2. Exactly one message. Print normally.
      [m] ->
        let log =
              MkLog
                { cmd = Just cmd,
                  msg = Types.fromUnlined $ timeMsg <> ": " <> m,
                  lvl,
                  mode
                }
         in ( Formatting.formatConsoleLog keyHide consoleLogging log,
              \fl -> Formatting.formatFileLog keyHide fl log
            )
      -- Received multiple messages (lines). Use custom formatters.
      (m : ms) ->
        let logs =
              (timeMsg :| m : ms) <&> \msg ->
                MkLog
                  { cmd = Just cmd,
                    msg = Types.fromUnlined msg,
                    lvl,
                    mode
                  }
         in ( Formatting.formatFinalConsoleLogs keyHide consoleLogging logs,
              \fl -> Formatting.formatFinalFileLogs keyHide fl logs
            )

    mode = LogModeFinish

printFinalResult ::
  forall m env e b.
  ( Exception e,
    HasAnyError env,
    HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasNotifyConfig env,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  TimeSpec ->
  Either e b ->
  m ()
printFinalResult totalTime result = withRegion Linear $ \r -> do
  Utils.whenLeft result $ \ex -> do
    let errMsg =
          mconcat
            [ "Encountered an exception. This is likely not an error in any ",
              "of the commands run but rather an error in Shrun itself: ",
              ShrunText.fromTextReplace $ displayExceptiont ex
            ]
        fatalLog =
          MkLog
            { cmd = Nothing,
              msg = Types.fromUnlined errMsg,
              lvl = LevelFatal,
              mode
            }
    Logging.putRegionLog r fatalLog

    -- update anyError
    setAnyErrorTrue

  timerFormat <- asks (view (_1 % #timerFormat) . getConsoleLogging @_ @(Region m))
  let totalTimeTxt =
        TimerFormat.formatRelativeTime
          timerFormat
          (Utils.timeSpecToRelTime totalTime)
      finalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined totalTimeTxt,
            lvl = LevelFinished,
            mode
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA =<< asks getAnyError
  let urgency = if anyError then Critical else Normal
      notifyBody = Notify.formatNotifyMessage totalTimeTxt []

  -- Sent off notif if NotifyAll or NotifyFinal is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif "Shrun Finished" notifyBody urgency
    Just NotifyFinal -> Notify.sendNotif "Shrun Finished" notifyBody urgency
    _ -> pure ()

  Logging.putRegionLog r finalLog
  where
    mode = LogModeFinish
{-# INLINEABLE printFinalResult #-}

counter ::
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasTimeout env,
    MonadIORef m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
counter = do
  -- HACK: This brief delay is so that our timer starts "last" i.e. after each
  -- individual command. This way the running timer console region is below all
  -- the commands' in the console.
  microsleep 100_000
  withRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- newIORef 0
    Utils.whileM_ (keepRunning r timer timeout) $ do
      sleep 1
      elapsed <- atomicModifyIORef' timer $ \t -> (t + 1, t + 1)
      logCounter r elapsed
{-# INLINEABLE counter #-}

logCounter ::
  forall m env.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    MonadReader env m,
    MonadSTM m
  ) =>
  Region m ->
  Natural ->
  m ()
logCounter region elapsed = do
  (consoleLogging, queue) <- asks (getConsoleLogging @_ @(Region m))

  keyHide <- asks (view #keyHide . getCommonLogging)
  let timerFormat = consoleLogging ^. #timerFormat
      msg = Types.fromUnlined $ TimerFormat.formatSeconds timerFormat elapsed
      lg =
        MkLog
          { cmd = Nothing,
            msg,
            lvl = LevelTimer,
            mode = LogModeSet
          }
      formatted = Formatting.formatConsoleLog keyHide consoleLogging lg
      regionLog = LogRegion LogModeSet region formatted
  Logging.regionLogToConsoleQueue queue regionLog
{-# INLINEABLE logCounter #-}

keepRunning ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadSTM m,
    MonadTime m
  ) =>
  Region m ->
  IORef Natural ->
  Maybe Timeout ->
  m Bool
keepRunning region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      log <- cancelRunningCommands "Timed out"
      Logging.putRegionLog region log
      pure False
    else pure True
{-# INLINEABLE keepRunning #-}

-- | Cancels running commands, returns a message to log.
cancelRunningCommands ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadSTM m
  ) =>
  UnlinedText ->
  m Log
cancelRunningCommands prefix = do
  keyHide <- asks (view #keyHide . getCommonLogging)
  allCmds <- asks getCommands
  completedCommandsTVar <- asks getCompletedCommands
  completedCommands <- readTVarA completedCommandsTVar

  -- update anyError
  setAnyErrorTrue

  let completedCommandsSet = Set.fromList $ toList completedCommands
      allCmdsSet = Set.fromList $ toList allCmds
      incompleteCmds = Set.difference allCmdsSet completedCommandsSet
      toTxtList acc cmd = LogFmt.displayCmd cmd keyHide : acc

      unfinishedCmds =
        ShrunText.intercalate ", "
          $ foldl' toTxtList [] incompleteCmds

  pure
    $ MkLog
      { cmd = Nothing,
        msg = Types.fromUnlined $ prefix <> ", cancelling remaining commands: " <> unfinishedCmds,
        lvl = LevelWarn,
        mode = LogModeFinish
      }
{-# INLINEABLE cancelRunningCommands #-}

timedOut :: Natural -> Maybe Timeout -> Bool
timedOut _ Nothing = False
timedOut timer (Just (MkTimeout t)) = timer > t

pollQueueToConsole ::
  ( HasCallStack,
    MonadMask m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  TBQueue (LogRegion (Region m)) ->
  m void
pollQueueToConsole queue = do
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ atomicReadWrite queue printConsoleLog
{-# INLINEABLE pollQueueToConsole #-}

printConsoleLog ::
  ( HasCallStack,
    MonadRegionLogger m
  ) =>
  LogRegion (Region m) ->
  m ()
printConsoleLog (LogNoRegion consoleLog) = logGlobal (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logRegion m r (consoleLog ^. #unConsoleLog)
{-# INLINEABLE printConsoleLog #-}

pollQueueToFile ::
  ( HasCallStack,
    MonadHandleWriter m,
    MonadMask m,
    MonadSTM m
  ) =>
  FileLoggingEnv ->
  m void
pollQueueToFile fileLogging = do
  forever
    $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    atomicReadWrite queue (logFile h)
  where
    MkFileLogOpened h queue = fileLogging ^. #file
{-# INLINEABLE pollQueueToFile #-}

logFile :: (HasCallStack, MonadHandleWriter m) => Handle -> FileLog -> m ()
logFile h = (\t -> hPutUtf8 h t *> hFlush h) . view #unFileLog
{-# INLINEABLE logFile #-}

-- | Reads from a queue and applies the function, if we receive a value.
-- Atomic in the sense that if a read is successful, then we will apply the
-- given function, even if an async exception is raised.
atomicReadWrite ::
  ( HasCallStack,
    MonadMask m,
    MonadSTM m
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> m b) ->
  m ()
atomicReadWrite queue logAction =
  mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
{-# INLINEABLE atomicReadWrite #-}

-- | Cancels running commands and prints a final log message about going
-- down. Intended to be used when shrun has been cancelled.
teardown ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    HasNotifyConfig env,
    MonadIORef m,
    MonadHandleWriter m,
    MonadNotify m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  m ()
teardown = withRegion Linear $ \r -> do
  commonLogging <- asks getCommonLogging
  (consoleLogging, _) <- asks (getConsoleLogging @env @(Region m))
  mFileLogging <- asks getFileLogging
  let keyHide = commonLogging ^. #keyHide
      errMsg = "Received cancel"
      fatalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined errMsg,
            lvl = LevelFatal,
            mode = LogModeFinish
          }

      fatalConsoleLog = Formatting.formatConsoleLog keyHide consoleLogging fatalLog

  cancelLog <- cancelRunningCommands errMsg
  let cancelConsoleLog = Formatting.formatConsoleLog keyHide consoleLogging cancelLog

  withRegion Linear $ \r2 -> logRegion LogModeFinish r2 (cancelConsoleLog ^. #unConsoleLog)

  logRegion LogModeFinish r (fatalConsoleLog ^. #unConsoleLog)

  let notifyBody = Notify.formatNotifyMessage errMsg []
      urgency = Critical

  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    -- If notifcations are on at all, send one
    Just _ -> Notify.sendNotif notifyBody "" urgency
    _ -> pure ()

  for_ mFileLogging $ \fl -> do
    cancelFileLog <- Formatting.formatFileLog keyHide fl cancelLog
    fatalFileLog <- Formatting.formatFileLog keyHide fl fatalLog
    logFile (fl ^. #file % #handle) cancelFileLog
    logFile (fl ^. #file % #handle) fatalFileLog
{-# INLINEABLE teardown #-}
