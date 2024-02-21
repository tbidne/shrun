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
import Data.Text qualified as T
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread as X (microsleep, sleep)
import Effects.Time (TimeSpec, withTiming)
import Shrun.Configuration.Env.Types
  ( FileLogging,
    HasAnyError (getAnyError),
    HasCommands (getCommands, getCompletedCmds),
    HasInit,
    HasLogging (getLogging),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    Logging,
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Timeout (Timeout (MkTimeout))
import Shrun.Data.TimerFormat qualified as TimerFormat
import Shrun.IO (Stderr (MkStderr), tryCommandLogging)
import Shrun.IO.Types (CommandResult (CommandFailure, CommandSuccess))
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
import Shrun.Notify.Types
  ( NotifyAction
      ( NotifyAll,
        NotifyCommand,
        NotifyFinal
      ),
  )
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import Shrun.Utils qualified as Utils

-- | Entry point
shrun ::
  ( HasAnyError env,
    HasCommands env,
    HasInit env,
    HasLogging env (Region m),
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
  logging <- asks getLogging

  -- always start console logger
  Async.withAsync pollQueueToConsole $ \consoleLogger -> do
    -- run commands, running file logger if requested
    maybe
      runCommands
      runWithFileLogging
      (logging ^. #fileLog)

    -- cancel consoleLogger, print remaining logs
    Async.cancel consoleLogger
    let consoleQueue = logging ^. #consoleLog
    flushTBQueueA consoleQueue >>= traverse_ printConsoleLog

    -- if any processes have failed, exit with an error
    anyError <- readTVarA =<< asks getAnyError
    when anyError exitFailure
  where
    runWithFileLogging fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueA fileQueue >>= traverse_ (logFile h)
        hFlush h
      where
        (h, fileQueue) = fileLogging ^. #log

    runCommands = do
      cmds <- asks getCommands
      let actions = Async.mapConcurrently_ runCommand cmds
          actionsWithTimer = Async.race_ actions counter

      (totalTime, result) <- withTiming $ tryAny actionsWithTimer
      printFinalResult totalTime result

runCommand ::
  forall m env.
  ( HasAnyError env,
    HasCommands env,
    HasInit env,
    HasLogging env (Region m),
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
  timerFormat <- view #timerFormat <$> (asks getLogging :: m (Logging (Region m)))

  let (urgency, msg', lvl, timeElapsed) = case cmdResult of
        CommandFailure t (MkStderr err) -> (Critical, ": " <> err, LevelError, t)
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

  logging :: Logging (Region m) <- asks getLogging
  let cmdNameTrunc = logging ^. #cmdNameTrunc
      keyHide = logging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide cmdNameTrunc cmd

  -- Sent off notif if NotifyAll or NotifyCommand is set
  cfg <- asks getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif (formattedCmd <> " Finished") timeMsg urgency
    Just NotifyCommand -> Notify.sendNotif (formattedCmd <> " Finished") timeMsg urgency
    _ -> pure ()

printFinalResult ::
  forall m env e b.
  ( Exception e,
    HasAnyError env,
    HasLogging env (Region m),
    HasNotifyConfig env,
    MonadCatch m,
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
              displayExceptiont ex
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

  timerFormat <- view #timerFormat <$> (asks getLogging :: m (Logging (Region m)))
  let totalTimeTxt = TimerFormat.formatRelativeTime timerFormat (Utils.timeSpecToRelTime totalTime)
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

counter ::
  ( HasAnyError env,
    HasCommands env,
    HasLogging env (Region m),
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

logCounter ::
  ( HasLogging env (Region m),
    MonadReader env m,
    MonadSTM m
  ) =>
  Region m ->
  Natural ->
  m ()
logCounter region elapsed = do
  logging <- asks getLogging
  let timerFormat = view #timerFormat logging
      msg = TimerFormat.formatSeconds timerFormat elapsed
      lg =
        MkLog
          { cmd = Nothing,
            msg,
            lvl = LevelTimer,
            mode = LogModeSet
          }
  Logging.regionLogToConsoleQueue region logging lg

keepRunning ::
  forall m env.
  ( HasAnyError env,
    HasCommands env,
    HasLogging env (Region m),
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
      keyHide <- asks (view #keyHide . getLogging @env @(Region m))
      allCmds <- asks getCommands
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- readTVarA completedCmdsTVar

      -- update anyError
      setAnyErrorTrue

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LogFmt.displayCmd cmd keyHide : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Logging.putRegionLog region
        $ MkLog
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
  ( HasLogging env (Region m),
    MonadMask m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  m void
pollQueueToConsole = do
  queue <- asks (view #consoleLog . getLogging)
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ atomicReadWrite queue printConsoleLog

printConsoleLog :: (MonadRegionLogger m) => LogRegion (Region m) -> m ()
printConsoleLog (LogNoRegion consoleLog) = logGlobal (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logRegion m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  ( MonadHandleWriter m,
    MonadMask m,
    MonadSTM m
  ) =>
  FileLogging ->
  m void
pollQueueToFile fileLogging = do
  forever
    $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    atomicReadWrite queue (logFile h)
  where
    (h, queue) = fileLogging ^. #log

logFile :: (MonadHandleWriter m) => Handle -> FileLog -> m ()
logFile h = (\t -> hPutUtf8 h t *> hFlush h) . view #unFileLog

-- | Reads from a queue and applies the function, if we receive a value.
-- Atomic in the sense that if a read is successful, then we will apply the
-- given function, even if an async exception is raised.
atomicReadWrite ::
  ( MonadMask m,
    MonadSTM m
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> m b) ->
  m ()
atomicReadWrite queue logAction =
  mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
