-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( ShellT,
    runShellT,
    shrun,
    TermException (..),
  )
where

import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.List qualified as L
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread (MonadThread (throwTo), ThreadId, myThreadId)
import Effects.System.Posix.Signals qualified as Signals
import Effects.Time (TimeSpec)
import Effects.Time qualified as Time
import Shrun.Command qualified as Command
import Shrun.Command.Types (CommandP1, CommandStatus (CommandRunning))
import Shrun.Configuration.Data.CommonLogging (CommonLoggingEnv)
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat (ProseCompact))
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat qualified as TimerFormat
import Shrun.Configuration.Data.Core.Timeout (Timeout (MkTimeout))
import Shrun.Configuration.Data.FileLogging
  ( FileLogOpened (MkFileLogOpened),
    FileLoggingEnv,
  )
import Shrun.Configuration.Data.Notify
  ( _NotifyActionsActiveCompleteAny,
    _NotifyActionsActiveStartAny,
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyActionComplete
      ( NotifyActionCompleteAll,
        NotifyActionCompleteCommand,
        NotifyActionCompleteFinal
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Configuration.Env.Types
  ( HasAnyError (getAnyError),
    HasCommands,
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit,
    HasLogging,
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    getReadCommandStatus,
    setAnyErrorTrue,
    setTimedOut,
    whenTimedOut,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Data.Text qualified as Text
import Shrun.IO
  ( CommandResult (CommandResultFailure, CommandResultSuccess),
    Stderr (MkStderr),
    tryCommandLogging,
  )
import Shrun.IO qualified
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
        LevelKilled,
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
import System.Posix.Signals qualified as Posix

-- | Entry point
shrun ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasLogging env m,
    HasNotifyConfig env,
    HasTimeout env,
    MonadAsync m,
    MonadEvaluate m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadNotify m,
    MonadPosixSignals m,
    MonadProcess m,
    MonadMVar m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  -- | .
  m ()
shrun = do
  startTime <- Time.getMonotonicTime
  displayRegions $ flip onMyAsync (teardown startTime) $ do
    mainTid <- myThreadId

    handleTerminate mainTid

    mFileLogging <- asks getFileLogging
    (_, consoleQueue, _) <- asks getConsoleLogging

    -- always start console logger
    Async.withAsync (pollQueueToConsole consoleQueue) $ \consoleLogger -> do
      -- run commands, running file logger if requested
      maybe
        (runCommands startTime)
        (runWithFileLogging startTime)
        mFileLogging

      -- cancel consoleLogger, print remaining logs
      Async.cancel consoleLogger
      flushTBQueueA consoleQueue >>= traverse_ printConsoleLog

      -- Need to run cleanup if we have timed out.
      whenTimedOut cleanupCommands

      -- if any processes have failed, exit with an error
      anyError <- readTVarA =<< asks getAnyError
      when anyError exitFailure
  where
    runWithFileLogging :: (HasCallStack) => Double -> FileLoggingEnv -> m ()
    runWithFileLogging startTime fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands startTime

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueA fileQueue >>= traverse_ (Logging.logFile h)
        hFlush h
      where
        MkFileLogOpened h fileQueue = fileLogging ^. #file

    runCommands :: (HasCallStack) => Double -> m ()
    runCommands startTime = do
      let actions = Command.runCommands (runCommand startTime)
          actionsWithTimer = Async.race_ actions counter

      result <- tryMySync actionsWithTimer
      endTime <- Time.getMonotonicTime
      printFinalResult (Time.fromSeconds $ endTime - startTime) result
{-# INLINEABLE shrun #-}

runCommand ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasLogging env m,
    HasNotifyConfig env,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadMask m,
    MonadNotify m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThread m,
    MonadTime m
  ) =>
  Double ->
  CommandP1 ->
  m ()
runCommand globalStartTime cmd = do
  cfg <- asks getNotifyConfig
  commonLogging <- asks getCommonLogging
  (consoleLogging, consoleQueue, _) <- asks (getConsoleLogging @env @(Region m))

  let commandNameTrunc = consoleLogging ^. #commandNameTrunc
      keyHide = commonLogging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide commandNameTrunc cmd

  case cfg ^? (_Just % #actions % _NotifyActionsActiveStartAny) of
    Just () -> do
      cmdStartTimeDouble <- Time.getMonotonicTime
      let cmdStartTime = Time.fromSeconds (cmdStartTimeDouble - globalStartTime)
          rt = Utils.timeSpecToRelTime cmdStartTime
          startTimeMsg = TimerFormat.formatRelativeTime ProseCompact rt
          notifyMsg = "Started after " <> startTimeMsg
      Notify.sendNotif
        (MonadNotify.fromUnlined $ formattedCmd <> "Started")
        (MonadNotify.fromUnlined notifyMsg)
        Normal
    _ -> pure ()

  cmdResult <- tryCommandLogging cmd

  let (urgency, consoleLog, mkFileLog, notifyMsg) =
        mkResultData commonLogging consoleLogging cmd cmdResult

  putCommandFinalLog consoleQueue consoleLog mkFileLog

  -- Sent off notif if NotifyActionCompleteAll or NotifyActionCompleteCommand is set
  case cfg ^? (_Just % #actions % _NotifyActionsActiveCompleteAny) of
    Just NotifyActionCompleteAll ->
      Notify.sendNotif (MonadNotify.fromUnlined $ formattedCmd <> " Finished") notifyMsg urgency
    Just NotifyActionCompleteCommand ->
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
      CommandResultFailure t (MkStderr []) -> (Critical, LevelError, t, ["<no error message>"])
      CommandResultFailure t (MkStderr errs) -> (Critical, LevelError, t, errs)
      CommandResultSuccess t -> (Normal, LevelSuccess, t, [])

    timeMsg = TimerFormat.formatRelativeTime timerFormat rt
    notifyMsg = Notify.formatNotifyMessage timeMsg messages

    -- NOTE: Strip leading and trailing "whitespace only" lines, as we do not
    -- want them in the final logs. We do want internal whitespace.
    --
    -- Note that this whitespace originally comes from when Handle uses
    -- 'ShrunText.fromText :: UnlinedText -> List UnlinedText', which removes
    -- newlines but does nothing else i.e. whitespace is preserved.
    --
    -- We attempted stripping there, but that has other unwanted consequences,
    -- like removing internal whitespace when we buffer logs. Hence we go
    -- with the least invasive method that does what we want: strip them
    -- from the final result here.
    messages' =
      L.dropWhileEnd Text.isWhitespace
        . L.dropWhile Text.isWhitespace
        $ messages

    (consoleLog, mMkFileLog) = case messages' of
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
         in ( Formatting.formatConsoleMultiLineLogs keyHide consoleLogging logs,
              \fl -> Formatting.formatFileMultiLineLogs keyHide fl logs
            )

    mode = LogModeFinish

printFinalResult ::
  forall m env e b.
  ( Exception e,
    HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env m,
    HasNotifyConfig env,
    MonadIORef m,
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

  -- print out any unfinished commands
  (mWaitingLog, mRunningLog) <- Logging.mkUnfinishedCmdLogs
  for_ mWaitingLog (Logging.putRegionMultiLineLog r)
  for_ mRunningLog (Logging.putRegionMultiLineLog r)

  totalTimeTxt <- formatTimeSpec totalTime
  let finalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA =<< asks getAnyError
  let urgency = if anyError then Critical else Normal
      notifyBody = Notify.formatNotifyMessage totalTimeTxt []

  -- Sent off notif if NotifyActionCompleteAll or NotifyActionCompleteFinal is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #actions % _NotifyActionsActiveCompleteAny) of
    Just NotifyActionCompleteAll -> Notify.sendNotif "Shrun Finished" notifyBody urgency
    Just NotifyActionCompleteFinal -> Notify.sendNotif "Shrun Finished" notifyBody urgency
    _ -> pure ()

  Logging.putRegionLog r finalLog
  where
    mode = LogModeFinish
{-# INLINEABLE printFinalResult #-}

formatTimeSpec ::
  forall env m.
  ( HasConsoleLogging env (Region m),
    MonadReader env m
  ) =>
  TimeSpec ->
  m UnlinedText
formatTimeSpec totalTime = do
  timerFormat <- asks (view (_1 % #timerFormat) . getConsoleLogging @_ @(Region m))
  pure
    $ TimerFormat.formatRelativeTime
      timerFormat
      (Utils.timeSpecToRelTime totalTime)
{-# INLINEABLE formatTimeSpec #-}

counter ::
  forall env m.
  ( HasAnyError env,
    HasCallStack,
    HasLogging env m,
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
    (_, _, regionVar) <- asks (getConsoleLogging @_ @(Region m))
    writeIORef regionVar (Just r)

    timeout <- asks getTimeout
    timer <- newIORef 0
    Utils.whileM_ (keepRunning r timer timeout) $ do
      sleep 1
      elapsed <- atomicModifyIORef' timer $ \t -> (t + 1, t + 1)
      logCounter r elapsed
    setTimedOut
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
  (consoleLogging, queue, _) <- asks (getConsoleLogging @_ @(Region m))

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
    HasLogging env m,
    MonadIORef m,
    MonadReader env m,
    MonadSTM m,
    MonadTime m
  ) =>
  Region m ->
  IORef Natural ->
  WithDisabled Timeout ->
  m Bool
keepRunning region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      -- update anyError
      setAnyErrorTrue
      let log =
            MkLog
              { cmd = Nothing,
                msg = "Timed out",
                lvl = LevelWarn,
                mode = LogModeFinish
              }
      Logging.putRegionLog region log
      pure False
    else pure True
{-# INLINEABLE keepRunning #-}

timedOut :: Natural -> WithDisabled Timeout -> Bool
timedOut _ Disabled = False
timedOut timer (With (MkTimeout t)) = timer > t

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
  forever $ Utils.atomicReadWrite queue printConsoleLog
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
    Utils.atomicReadWrite queue (Logging.logFile h)
  where
    MkFileLogOpened h queue = fileLogging ^. #file
{-# INLINEABLE pollQueueToFile #-}

-- | Cancels running commands and prints a final log message about going
-- down. Intended to be used when shrun has been cancelled.
teardown ::
  forall m env.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env m,
    HasNotifyConfig env,
    MonadCatch m,
    MonadIORef m,
    MonadHandleWriter m,
    MonadNotify m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  Double ->
  m ()
teardown startTime = do
  endTime <- Time.getMonotonicTime
  let totalTime = Time.fromSeconds $ endTime - startTime
  timeFormatted <- formatTimeSpec totalTime

  let cancelTasksMsg = "Received cancel"
      finalErrMsg = cancelTasksMsg <> " after running for: " <> timeFormatted

  -- update anyError
  setAnyErrorTrue

  (mWaitingLog, mRunningLog) <- Logging.mkUnfinishedCmdLogs

  -- NOTE: Manual logging because the logging queues have been shutdown at this
  -- point. We must write to the console (logRegion) and file (logFile)
  -- directly.

  -- 1. Send message about cancelling commands.
  traverse_ Logging.putRegionMultiLineLogDirect mWaitingLog
  traverse_ Logging.putRegionMultiLineLogDirect mRunningLog

  -- Clean up remaining commands.
  cleanupCommands

  let notifyBody = Notify.formatNotifyMessage finalErrMsg []

  -- 2. Send finished message.
  let finalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined finalErrMsg,
            lvl = LevelKilled,
            mode = LogModeFinish
          }

  Logging.putRegionLogDirect finalLog

  -- 3. Send notification
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #actions % _NotifyActionsActiveCompleteAny) of
    -- If complete notifcations are on at all, send one
    Just _ -> Notify.sendNotif notifyBody "" Critical
    _ -> pure ()
{-# INLINEABLE teardown #-}

-- | Installs a handler for SIGTERM, so shrun can be cancelled with kill -15.
-- The signal is logged then rethrown to the main thread as TermException,
-- which ensures that cleanup is handled normally (i.e. subcommands killed).
-- By default, subthreads are __not__ killed when the RTS handles SIGTERM.
handleTerminate ::
  forall m env.
  ( HasCallStack,
    HasLogging env m,
    MonadHandleWriter m,
    MonadPosixSignals m,
    MonadRegionLogger m,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  ThreadId ->
  m ()
handleTerminate tid = do
  let handler = Signals.CatchInfo $ \si -> do
        let errMsg =
              "Received terminate signal: "
                <> Text.unsafeUnlinedText (showt (Posix.siginfoSignal si))
            baseLog =
              MkLog
                { cmd = Nothing,
                  msg = Types.fromUnlined errMsg,
                  lvl = LevelFatal,
                  mode = LogModeFinish
                }

        Logging.putRegionLogDirect baseLog

        -- Need to throw exception to main thread since this handler is run
        -- in a different thread.
        throwTo tid MkTermException

  void $ Signals.installHandler Posix.sigTERM handler Nothing

-- NOTE: [Command cleanup]
--
-- When shrun is going to terminate prematurely (e.g. killed externally or
-- a fatal exception is encountered), we want all subcommands to terminate
-- as well. We generally rely on our libraries to handle this automatically:
--
--   - async ensures an exception in the main thread is rethrown to all
--     subthreads.
--
--   - process forwards this exception to the running command.
--
-- While this is often enough, unfortunately there are some situations where
-- it is not. First, note that command running is complicated by the fact
-- that we are running through the shell, so e.g. "shrun 'some command'"
-- actually runs "/bin/sh -c 'some command'", which in turn runs
-- 'some command' in a platform-specific way.
--
-- For example, my local (linux) machine and CI OSX appear to immediately
-- terminate the /bin/sh command, and run 'some command' directly, whereas
-- CI Linux has both running.
--
-- Unfortunately, while an exception will terminate the /bin/sh command
-- on CI Linux, this exception does _not_ get proprogated to the underlying
-- 'some command'. To make matters worse, 'some command' has its parent PID
-- reassigned to PID 1, meaning we no longer have any connection to this
-- process.
--
-- To combat this, when we launch a command, we immediately store its PID
-- and any child PIDs in our command status map. Then, we attempt to kill all
-- of this upon cleanup. While this is overkill on some platforms,
-- it is necessary for CI linux (and presumably others), and does not appear
-- to be harmful. Note that this requires the following utilities:
--
--   - kill
--   - pgrep
--
-- Note that this _does not_ replace the need for commands to implement their
-- own cleanup as needed. That is, if a command spawns its own processes then
-- is that command's responsibilities to clean up these commands. Our cleanup
-- logic is only intended for handling the case where our spawned /bin/sh
-- does not forward the kill signal to its child.
cleanupCommands ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  m ()
cleanupCommands = do
  commandsStatus <- getReadCommandStatus

  for_ commandsStatus $ \(_cmd, status) -> do
    case status of
      CommandRunning (mPid, childPids) -> do
        -- 1. Kill the commands' children that were immediately spawned.
        -- This is the primary 'fix', as it is what happens on CI Linux,
        -- at least. This ensures we kill some_command when our /bin/sh
        -- commands do not forward the signal.
        --
        -- For platforms that end the /bin/sh immediately, this generally
        -- does nothing (which is fine, as then some_command will receive
        -- the normal terminate signal).
        Shrun.IO.killPids childPids

        -- 2. Needed for CI OSX to pass the test_script.sh test. That is,
        -- the spawned sleep commands are not cancelled. We have the log:
        --
        --   [Debug] Failed finding child pids of '13456': out: '', err: ' '
        --
        -- Where 13456 is the PPID of sleep command i.e. the PID of the script.
        -- This is correct, but we fail to find the child pids anyway.
        -- Either there is a bug in getChildPids, or the child's PPID has
        -- been reassigned by the time we run getChildPids, which seems
        -- more likely.
        Shrun.IO.killChildPids mPid

        -- 3. Needed for CI Linux to pass the test_script.sh test.
        for_ childPids killChildPids
      _ -> pure ()
  where
    killChildPids = Shrun.IO.killChildPids . Just
