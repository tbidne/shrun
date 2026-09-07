{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( shrun,
    TermException (..),
  )
where

import Data.List qualified as L
import Effectful.Concurrent.Async qualified as Async
import Effectful.Time.Dynamic qualified as Time
import Shrun.Cleanup qualified as Cleanup
import Shrun.Command qualified as Command
import Shrun.Command.Types (CommandP1)
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
  ( NotificationEnv,
    _NotifyActionsActiveCompleteAny,
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
    formatTimeSpec,
    setAnyErrorTrue,
    setTimedOut,
    whenTimedOut,
  )
import Shrun.Data.Text qualified as ShrunText
import Shrun.Data.Text qualified as Text
import Shrun.IO
  ( CommandResult (CommandResultFailure, CommandResultSuccess),
    Stderr (MkStderr),
    tryCommandLogging,
  )
import Shrun.IO.Signals qualified as Signals
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Formatting qualified as LogFmt
import Shrun.Logging.RegionLogger
  ( RegionLogger,
    displayRegions,
    logGlobal,
    logRegion,
    withRegion,
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
import Shrun.Notify (NotifyMessage)
import Shrun.Notify qualified as Notify
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Entry point
shrun ::
  forall env notifyEnv r es.
  ( Eq r,
    HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasLogging env r,
    HasNotifyConfig env notifyEnv,
    HasTimeout env,
    Concurrent :> es,
    HandleReader :> es,
    HandleWriter :> es,
    Prim :> es,
    Notify notifyEnv :> es,
    PathReader :> es,
    PathWriter :> es,
    PosixFiles :> es,
    PosixSignals :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  -- | .
  Eff es ()
shrun = do
  -- install handler that turns SIGTERM into an exception in the main thread.
  Signals.installTermHandler @env @r

  startTime <- Time.getMonotonicTime

  Utils.withHiddenInput $ displayRegions @r $ flip onMyAsync (Cleanup.teardown @env @r @notifyEnv startTime) $ do
    mFileLogging <- asks @env getFileLogging
    (_, consoleQueue, _) <- asks (getConsoleLogging @env @r)

    -- always start console logger
    Async.withAsync (pollQueueToConsole consoleQueue) $ \consoleLogger -> do
      -- run commands, running file logger if requested
      maybe
        (runCommands startTime)
        (runWithFileLogging startTime)
        mFileLogging

      -- cancel consoleLogger, print remaining logs
      Async.cancel consoleLogger
      flushTBQueueA' consoleQueue >>= traverse_ printConsoleLog

      -- Need to run cleanup if we have timed out.
      whenTimedOut @env (Cleanup.cleanupCommands @env @r)

      -- One final attempt draining stdin.
      Utils.drainStdin

      -- if any processes have failed, exit with an error
      anyError <- readTVarA' =<< asks @env getAnyError
      when anyError exitFailure
  where
    runWithFileLogging :: (HasCallStack) => Double -> FileLoggingEnv -> Eff es ()
    runWithFileLogging startTime fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands startTime

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueA' fileQueue >>= traverse_ (Logging.logFile h)
        liftLocked hFlush h
      where
        MkFileLogOpened h _ fileQueue = fileLogging ^. #file

    runCommands :: (HasCallStack) => Double -> Eff es ()
    runCommands startTime = do
      let actions = Command.runCommands @env @r (runCommand @env @notifyEnv @r startTime)
          actionsWithTimer =
            actions
              `Async.race_` (counter @env @r)
              `Async.race_` drainStdinLoop

      result <- tryMySync actionsWithTimer
      endTime <- Time.getMonotonicTime
      printFinalResult @env @notifyEnv @r (Time.fromSeconds $ endTime - startTime) result

runCommand ::
  forall env notifyEnv r es.
  ( Eq r,
    HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasLogging env r,
    HasNotifyConfig env notifyEnv,
    Concurrent :> es,
    HandleReader :> es,
    HandleWriter :> es,
    Prim :> es,
    Notify notifyEnv :> es,
    PathReader :> es,
    PathWriter :> es,
    PosixFiles :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Double ->
  CommandP1 ->
  Eff es ()
runCommand globalStartTime cmd = do
  mCfg <- asks @env (getNotifyConfig @_ @notifyEnv)
  commonLogging <- asks @env getCommonLogging
  (consoleLogging, consoleQueue, _) <- asks @env (getConsoleLogging @env @r)

  let commandNameTrunc = consoleLogging ^. #commandNameTrunc
      cmdIndex = commonLogging ^. #commandIndex
      keyHide = commonLogging ^. #keyHide
      formattedCmd = LogFmt.formatCommand cmdIndex keyHide commandNameTrunc cmd

  case mCfg ^? (_Just % #actions % _NotifyActionsActiveStartAny) of
    Just () -> do
      cmdStartTimeDouble <- Time.getMonotonicTime
      let cmdStartTime = Time.fromSeconds (cmdStartTimeDouble - globalStartTime)
          rt = Utils.timeSpecToRelTime cmdStartTime
          startTimeMsg = TimerFormat.formatRelativeTime ProseCompact rt
          notifyMsg = "Started after " <> startTimeMsg
      Notify.sendNotif
        @env
        @r
        @notifyEnv
        (Notify.fromUnlined $ formattedCmd <> " Started")
        (Notify.fromUnlined notifyMsg)
        NotifyUrgencyNormal
    _ -> pure ()

  cmdResult <- tryCommandLogging @env @r cmd

  let (mkUrgency, mkConsoleLog, mkFileLog, notifyMsg) =
        mkResultData @env commonLogging consoleLogging cmd cmdResult

  putCommandFinalLog @env consoleQueue mkConsoleLog mkFileLog

  -- Sent off notif if NotifyActionCompleteAll or NotifyActionCompleteCommand is set
  for_ mCfg $ \cfg -> do
    let urgency = mkUrgency cfg

    case cfg ^? (#actions % _NotifyActionsActiveCompleteAny) of
      Just NotifyActionCompleteAll ->
        Notify.sendNotif @env @r @notifyEnv (Notify.fromUnlined $ formattedCmd <> " Finished") notifyMsg urgency
      Just NotifyActionCompleteCommand ->
        Notify.sendNotif @env @r @notifyEnv (Notify.fromUnlined $ formattedCmd <> " Finished") notifyMsg urgency
      _ -> pure ()

-- | Prints the final log from the command (i.e. success/error message).
-- Has different log depending on the output (i.e. if we should log
-- multiple lines).
putCommandFinalLog ::
  forall env r es.
  ( HasCallStack,
    HasFileLogging env,
    Concurrent :> es,
    Reader env :> es,
    RegionLogger r :> es
  ) =>
  TBQueue (LogRegion r) ->
  Eff es ConsoleLog ->
  (FileLoggingEnv -> Eff es FileLog) ->
  Eff es ()
putCommandFinalLog consoleQueue mkConsoleLog mkFileLog = do
  consoleLog <- mkConsoleLog
  withRegion Linear $ \r -> writeTBQueueA' consoleQueue (LogRegion mode r consoleLog)

  mFileLogging <- asks @env getFileLogging
  for_ mFileLogging $ \fl -> do
    fileLog <- mkFileLog fl
    Logging.logToFileQueue fl fileLog
  where
    mode = LogModeFinish

-- | All of the command result data needed for final log.
type CommandResultData notifyEnv m =
  Tuple4
    -- Urgency level for notifs
    (NotificationEnv notifyEnv -> NotifyUrgency)
    -- Console log
    (m ConsoleLog)
    -- File log, if active
    (FileLoggingEnv -> m FileLog)
    -- Notif body
    NotifyMessage

-- | Gets log data from CommandResult.
mkResultData ::
  forall env notifyEnv es.
  ( HasCallStack,
    HasCommands env,
    Concurrent :> es,
    Reader env :> es,
    Time :> es
  ) =>
  CommonLoggingEnv ->
  ConsoleLoggingEnv ->
  CommandP1 ->
  CommandResult ->
  CommandResultData notifyEnv (Eff es)
mkResultData commonLogging consoleLogging cmd cmdResult =
  (urgency, consoleLog, mMkFileLog, notifyMsg)
  where
    timerFormat = consoleLogging ^. #timerFormat
    cmdIndex = commonLogging ^. #commandIndex
    keyHide = commonLogging ^. #keyHide

    mkErrUrgency cfg = cfg ^. #errUrgency % #unNotifyErrUrgency

    (urgency, lvl, rt, messages) = case cmdResult of
      CommandResultFailure t (MkStderr []) -> (mkErrUrgency, LevelError, t, ["<no error message>"])
      CommandResultFailure t (MkStderr errs) -> (mkErrUrgency, LevelError, t, errs)
      CommandResultSuccess t -> (const NotifyUrgencyNormal, LevelSuccess, t, [])

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
         in ( Formatting.formatConsoleLog @env cmdIndex keyHide consoleLogging log,
              \fl -> Formatting.formatFileLog @env cmdIndex keyHide fl log
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
         in ( Formatting.formatConsoleLog @env cmdIndex keyHide consoleLogging log,
              \fl -> Formatting.formatFileLog @env cmdIndex keyHide fl log
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
         in ( Formatting.formatConsoleMultiLineLogs @env cmdIndex keyHide consoleLogging logs,
              \fl -> Formatting.formatFileMultiLineLogs @env cmdIndex keyHide fl logs
            )

    mode = LogModeFinish

printFinalResult ::
  forall env notifyEnv r e b es.
  ( Exception e,
    HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env r,
    HasNotifyConfig env notifyEnv,
    Concurrent :> es,
    Notify notifyEnv :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  TimeSpec ->
  Either e b ->
  Eff es ()
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

    Logging.putRegionLog @env r fatalLog

    -- update anyError
    setAnyErrorTrue @env

  -- print out any unfinished commands
  (mWaitingLog, mRunningLog) <- Logging.mkUnfinishedCmdLogs @env
  for_ mWaitingLog (Logging.putRegionMultiLineLog @env r)
  for_ mRunningLog (Logging.putRegionMultiLineLog @env r)

  totalTimeTxt <- formatTimeSpec @env @r totalTime
  let finalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA' =<< asks @env getAnyError

  -- Sent off notif if NotifyActionCompleteAll or NotifyActionCompleteFinal is set
  mCfg <- asks @env (getNotifyConfig @_ @notifyEnv)

  for_ mCfg $ \cfg -> do
    let urgency
          | anyError = cfg ^. #errUrgency % #unNotifyErrUrgency
          | otherwise = NotifyUrgencyNormal
        notifyBody = Notify.formatNotifyMessage totalTimeTxt []

    case cfg ^? (#actions % _NotifyActionsActiveCompleteAny) of
      Just NotifyActionCompleteAll -> Notify.sendNotif @env @r @notifyEnv "Shrun Finished" notifyBody urgency
      Just NotifyActionCompleteFinal -> Notify.sendNotif @env @r @notifyEnv "Shrun Finished" notifyBody urgency
      _ -> pure ()

  Logging.putRegionLog @env @r r finalLog
  where
    mode = LogModeFinish

counter ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env r,
    HasTimeout env,
    Concurrent :> es,
    Prim :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Eff es ()
counter = do
  -- HACK: This brief delay is so that our timer starts "last" i.e. after each
  -- individual command. This way the running timer console region is below all
  -- the commands' in the console.
  microsleep 100_000
  withRegion Linear $ \r -> do
    (_, _, regionVar) <- asks @env (getConsoleLogging @_ @r)
    writeIORef regionVar (Just r)

    timeout <- asks @env getTimeout
    timer <- newIORef 0
    Utils.whileM_ (keepRunning @env r timer timeout) $ do
      sleep 1
      elapsed <- atomicModifyIORef timer $ \t -> (t + 1, t + 1)
      logCounter @env r elapsed

    setTimedOut @env

-- | Periodically attempts to read stdin, so any entered keystrokes are
-- thrown away. Does not apply to commands that spawn sudo, sadly.
drainStdinLoop ::
  forall es void.
  ( Concurrent :> es,
    HandleReader :> es
  ) =>
  Eff es void
drainStdinLoop = go
  where
    go = do
      Utils.drainStdin
      -- Choosing a good drain period is pretty ad-hoc. We have two goals:
      --
      --   1. Prevent stdin from appearing after shrun exits.
      --   2. Prevent stdin from being held in memory for shrun's duration.
      --
      -- 1 is mostly accomplished by having a single drain at the end, so we
      -- theoretically do not need this loop at all. The loop is only
      -- necessary for 2.
      --
      -- However, it could be the case that the amount of stdin is greater
      -- than a single drain amount, in which case having periodic drains
      -- would be a mitigation. This is pretty unlikely as any stdin is
      -- likely to be a mistake (i.e. merely a few keystrokes), but it is
      -- worth mentioning.
      --
      -- We therefore have the following considerations:
      --
      --   - The importance of periodic drains is pretty low, and we do
      --     not want performance to be impacted.
      --
      --   - This is likely only a benefit to long-lived commands i.e.
      --     minutes.
      --
      --   - Hence while _some_ periodic drain is probably a good idea,
      --     we should make the period long enough that it is not
      --     noticeable.
      --
      sleep 60
      go

logCounter ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    Concurrent :> es,
    Reader env :> es
  ) =>
  r ->
  Natural ->
  Eff es ()
logCounter region elapsed = do
  (consoleLogging, queue, _) <- asks @env (getConsoleLogging @_ @r)
  commonLogging <- asks @env getCommonLogging

  let cmdIndex = commonLogging ^. #commandIndex
      keyHide = commonLogging ^. #keyHide

  let timerFormat = consoleLogging ^. #timerFormat
      msg = Types.fromUnlined $ TimerFormat.formatSeconds timerFormat elapsed
      lg =
        MkLog
          { cmd = Nothing,
            msg,
            lvl = LevelTimer,
            mode = LogModeSet
          }
  formatted <- Formatting.formatConsoleLog @env cmdIndex keyHide consoleLogging lg
  let regionLog = LogRegion LogModeSet region formatted
  Logging.regionLogToConsoleQueue queue regionLog

keepRunning ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    Prim :> es,
    Reader env :> es,
    Time :> es
  ) =>
  r ->
  IORef Natural ->
  WithDisabled Timeout ->
  Eff es Bool
keepRunning region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      -- update anyError
      setAnyErrorTrue @env
      let log =
            MkLog
              { cmd = Nothing,
                msg = "Timed out",
                lvl = LevelWarn,
                mode = LogModeFinish
              }
      Logging.putRegionLog @env region log
      pure False
    else pure True

timedOut :: Natural -> WithDisabled Timeout -> Bool
timedOut _ Disabled = False
timedOut timer (With (MkTimeout t)) = timer > t

pollQueueToConsole ::
  ( HasCallStack,
    Concurrent :> es,
    RegionLogger r :> es
  ) =>
  TBQueue (LogRegion r) ->
  Eff es void
pollQueueToConsole queue = do
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ Utils.atomicReadWrite queue printConsoleLog

printConsoleLog ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  LogRegion r ->
  Eff es ()
printConsoleLog (LogNoRegion consoleLog) = logGlobal @r (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logRegion m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  ( HasCallStack,
    Concurrent :> es,
    HandleWriter :> es
  ) =>
  FileLoggingEnv ->
  Eff es void
pollQueueToFile fileLogging = do
  forever
    $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    Utils.atomicReadWrite queue (Logging.logFile h)
  where
    MkFileLogOpened h _ queue = fileLogging ^. #file
