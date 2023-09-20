{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( shrun,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.Static as X (microsleep, sleep)
import Effectful.Time.Dynamic (TimeSpec, withTiming)
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
import Shrun.Logging.RegionLogger (RegionLoggerDynamic)
import Shrun.Logging.RegionLogger qualified as RegionLogger
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
import Shrun.Notify.Notify (NotifyDynamic)
import Shrun.Notify.Types (_NotifyCommand)
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Entry point
shrun ::
  forall env r es.
  ( Concurrent :> es,
    HasAnyError env,
    HasCommands env,
    HasInit env,
    HasLogging env r,
    HasNotifyConfig env,
    HasTimeout env,
    HandleReaderStatic :> es,
    HandleWriterStatic :> es,
    IORefStatic :> es,
    NotifyDynamic :> es,
    Reader env :> es,
    RegionLoggerDynamic r :> es,
    TimeDynamic :> es,
    TypedProcess :> es
  ) =>
  -- | .
  Eff es ()
shrun = RegionLogger.displayRegions @r $ do
  logging :: Logging r <- asks @env getLogging

  -- always start console logger
  Async.withAsync (pollQueueToConsole @env @r) $ \consoleLogger -> do
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
    anyError <- readTVarA =<< asks @env getAnyError
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
      cmds <- asks @env getCommands
      let actions = Async.mapConcurrently_ (runCommand @env @r) cmds
          actionsWithTimer = Async.race_ actions (counter @env @r)

      (totalTime, result) <- withTiming $ tryAny actionsWithTimer
      printFinalResult @env @r totalTime result

runCommand ::
  forall env r es.
  ( Concurrent :> es,
    HasAnyError env,
    HasCommands env,
    HasInit env,
    HasLogging env r,
    HasNotifyConfig env,
    HandleReaderStatic :> es,
    IORefStatic :> es,
    NotifyDynamic :> es,
    Reader env :> es,
    RegionLoggerDynamic r :> es,
    TimeDynamic :> es,
    TypedProcess :> es
  ) =>
  CommandP1 ->
  Eff es ()
runCommand cmd = do
  cmdResult <- tryCommandLogging @env @r cmd
  timerFormat <- view #timerFormat <$> (asks @env getLogging :: Eff es (Logging r))

  let (urgency, msg', lvl, timeElapsed) = case cmdResult of
        CommandFailure t (MkStderr err) -> (Critical, ": " <> err, LevelError, t)
        CommandSuccess t -> (Normal, "", LevelSuccess, t)
      timeMsg = TimerFormat.formatRelativeTime timerFormat timeElapsed <> msg'

  RegionLogger.withRegion @r Linear $ \r ->
    Logging.putRegionLog @env r
      $ MkLog
        { cmd = Just cmd,
          msg = timeMsg,
          lvl,
          mode = LogModeFinish
        }

  logging :: Logging r <- asks @env getLogging
  let cmdNameTrunc = logging ^. #cmdNameTrunc
      keyHide = logging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide cmdNameTrunc cmd

  -- Sent off notif if NotifyCommand is set
  cfg <- asks @env getNotifyConfig
  when (is (_Just % #action % _NotifyCommand) cfg)
    $ Notify.sendNotif @env @r (formattedCmd <> " Finished") timeMsg urgency

printFinalResult ::
  forall env r es e b.
  ( Concurrent :> es,
    Exception e,
    HasAnyError env,
    HasLogging env r,
    HasNotifyConfig env,
    NotifyDynamic :> es,
    Reader env :> es,
    RegionLoggerDynamic r :> es,
    TimeDynamic :> es
  ) =>
  TimeSpec ->
  Either e b ->
  Eff es ()
printFinalResult totalTime result = RegionLogger.withRegion Linear $ \r -> do
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
    Logging.putRegionLog @env @r r fatalLog

    -- update anyError
    setAnyErrorTrue @env

  timerFormat <- view #timerFormat <$> (asks @env getLogging :: Eff es (Logging r))
  let totalTimeTxt = TimerFormat.formatRelativeTime timerFormat (Utils.timeSpecToRelTime totalTime)
      finalLog =
        MkLog
          { cmd = Nothing,
            msg = totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA =<< asks @env getAnyError
  let urgency = if anyError then Critical else Normal

  -- Sent off notif if notifications are on
  cfg <- asks @env getNotifyConfig
  when (is _Just cfg)
    $ Notify.sendNotif @env @r "Shrun Finished" totalTimeTxt urgency

  Logging.putRegionLog @env r finalLog

counter ::
  forall env r es.
  ( Concurrent :> es,
    HasAnyError env,
    HasCommands env,
    HasLogging env r,
    HasTimeout env,
    IORefStatic :> es,
    Reader env :> es,
    RegionLoggerDynamic r :> es,
    TimeDynamic :> es
  ) =>
  Eff es ()
counter = do
  -- HACK: This brief delay is so that our timer starts "last" i.e. after each
  -- individual command. This way the running timer console region is below all
  -- the commands' in the console.
  microsleep 100_000
  RegionLogger.withRegion @r Linear $ \r -> do
    timeout <- asks @env getTimeout
    timer <- newIORef 0
    Utils.whileM_ (keepRunning @env r timer timeout) $ do
      sleep 1
      elapsed <- atomicModifyIORef' timer $ \t -> (t + 1, t + 1)
      logCounter @env r elapsed

logCounter ::
  forall env r es.
  ( Concurrent :> es,
    HasLogging env r,
    Reader env :> es
  ) =>
  r ->
  Natural ->
  Eff es ()
logCounter region elapsed = do
  logging <- asks @env getLogging
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
  forall env r es.
  ( Concurrent :> es,
    HasAnyError env,
    HasCommands env,
    HasLogging env r,
    IORefStatic :> es,
    Reader env :> es,
    TimeDynamic :> es
  ) =>
  r ->
  IORef Natural ->
  Maybe Timeout ->
  Eff es Bool
keepRunning region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      keyHide <- asks (view #keyHide . getLogging @env @r)
      allCmds <- asks @env getCommands
      completedCmdsTVar <- asks @env getCompletedCmds
      completedCmds <- readTVarA completedCmdsTVar

      -- update anyError
      setAnyErrorTrue @env

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LogFmt.displayCmd cmd keyHide : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Logging.putRegionLog @env region
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
  forall env r es void.
  ( Concurrent :> es,
    HasLogging env r,
    Reader env :> es,
    RegionLoggerDynamic r :> es
  ) =>
  Eff es void
pollQueueToConsole = do
  queue :: TBQueue (LogRegion r) <- asks (view #consoleLog . getLogging @env @r)
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ atomicReadWrite queue (printConsoleLog @r)

printConsoleLog :: forall r es. (RegionLoggerDynamic r :> es) => LogRegion r -> Eff es ()
printConsoleLog (LogNoRegion consoleLog) = RegionLogger.logGlobal @r (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = RegionLogger.logRegion m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  forall es void.
  ( Concurrent :> es,
    HandleWriterStatic :> es
  ) =>
  FileLogging ->
  Eff es void
pollQueueToFile fileLogging = do
  forever
    $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    atomicReadWrite queue (logFile @es h)
  where
    (h, queue) = fileLogging ^. #log

logFile :: (HandleWriterStatic :> es) => Handle -> FileLog -> Eff es ()
logFile h = (\t -> hPutUtf8 h t *> hFlush h) . view #unFileLog

-- | Reads from a queue and applies the function, if we receive a value.
-- Atomic in the sense that if a read is successful, then we will apply the
-- given function, even if an async exception is raised.
atomicReadWrite ::
  ( Concurrent :> es
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> Eff es b) ->
  Eff es ()
atomicReadWrite queue logAction =
  mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
