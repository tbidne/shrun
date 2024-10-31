-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( ShellT,
    runShellT,
    shrun,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.HashSet qualified as Set
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread as X (microsleep, sleep)
import Effects.Time (TimeSpec, withTiming)
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
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO
  ( CommandResult (CommandFailure, CommandSuccess),
    Stderr (MkStderr),
    tryCommandLogging,
  )
import Shrun.Logging qualified as Logging
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
  ( FileLog,
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
import Shrun.Notify qualified as Notify
import Shrun.Notify.MonadNotify (MonadNotify)
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
shrun = displayRegions $ do
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
    {-# INLINEABLE runWithFileLogging #-}

    runCommands :: (HasCallStack) => m ()
    runCommands = do
      cmds <- asks getCommands
      let actions = Async.mapConcurrently_ runCommand cmds
          actionsWithTimer = Async.race_ actions counter

      (totalTime, result) <- withTiming $ trySync actionsWithTimer
      printFinalResult totalTime result
    {-# INLINEABLE runCommands #-}
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
  (consoleLogging, _) <- asks (getConsoleLogging @env @(Region m))

  let timerFormat = consoleLogging ^. #timerFormat
      (urgency, msg', lvl, timeElapsed) = case cmdResult of
        -- see NOTE: [Text Line Concatentation] for how we combine the
        -- multiple texts back into a single err.
        CommandFailure t (MkStderr errs) ->
          let errMsg = ShrunText.concat errs
           in (Critical, ": " <> errMsg, LevelError, t)
        CommandSuccess t -> (Normal, "", LevelSuccess, t)
      timeMsg = TimerFormat.formatRelativeTime timerFormat timeElapsed <> msg'

  withRegion Linear $ \r ->
    Logging.putRegionLog r
      $ MkLog
        { cmd = Just cmd,
          msg = timeMsg,
          lvl,
          mode = LogModeFinish
        }

  let commandNameTrunc = consoleLogging ^. #commandNameTrunc
      keyHide = commonLogging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide commandNameTrunc cmd

  -- Sent off notif if NotifyAll or NotifyCommand is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif (formattedCmd <> " Finished") timeMsg urgency
    Just NotifyCommand -> Notify.sendNotif (formattedCmd <> " Finished") timeMsg urgency
    _ -> pure ()
{-# INLINEABLE runCommand #-}

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
              msg = errMsg,
              lvl = LevelFatal,
              mode = LogModeFinish
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
            msg = totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA =<< asks getAnyError
  let urgency = if anyError then Critical else Normal

  -- Sent off notif if NotifyAll or NotifyFinal is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif "Shrun Finished" totalTimeTxt urgency
    Just NotifyFinal -> Notify.sendNotif "Shrun Finished" totalTimeTxt urgency
    _ -> pure ()

  Logging.putRegionLog r finalLog
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
  timerFormat <- asks (view (_1 % #timerFormat) . getConsoleLogging @_ @(Region m))

  let msg = TimerFormat.formatSeconds timerFormat elapsed
      lg =
        MkLog
          { cmd = Nothing,
            msg,
            lvl = LevelTimer,
            mode = LogModeSet
          }
  Logging.regionLogToConsoleQueue region lg
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

      Logging.putRegionLog region
        $ MkLog
          { cmd = Nothing,
            msg = "Timed out, cancelling remaining commands: " <> unfinishedCmds,
            lvl = LevelWarn,
            mode = LogModeFinish
          }
      pure False
    else pure True
{-# INLINEABLE keepRunning #-}

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
