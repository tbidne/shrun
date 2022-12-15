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

import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time.Relative (formatRelativeTime, formatSeconds)
import Effects.MonadSTM (MonadTBQueue (tryReadTBQueueM))
import Effects.MonadThread as X (MonadThread (microsleep), sleep)
import Effects.MonadTime (MonadTime (..), TimeSpec, withTiming)
import Shrun.Configuration.Env.Types
  ( FileLogging,
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.Timeout (Timeout (..))
import Shrun.IO (Stderr (..), tryCommandLogging)
import Shrun.Logging
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
    LogRegion (..),
    MonadRegionLogger (..),
  )
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as LogFmt
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import Shrun.Utils qualified as Utils
import UnliftIO.Async qualified as Async

-- | Entry point
--
-- @since 0.1
shrun ::
  ( HasCommands env,
    HasLogging env (Region m),
    HasTimeout env,
    MonadCallStack m,
    MonadIORef m,
    MonadFsWriter m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m,
    MonadThread m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  m ()
shrun = displayRegions $ do
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
        flushTBQueueM fileQueue >>= traverse_ (logFile h)
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
  ( HasCommands env,
    HasLogging env (Region m),
    MonadCallStack m,
    MonadIORef m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  Command ->
  m ()
runCommand cmd = do
  cmdResult <- tryCommandLogging cmd

  withRegion Linear $ \r -> do
    let (msg', lvl, timeElapsed) = case cmdResult of
          Left (t, MkStderr err) -> (": " <> err, LevelError, t)
          Right t -> ("", LevelSuccess, t)
    Logging.putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = T.pack (formatRelativeTime timeElapsed) <> msg',
          lvl,
          mode = LogModeFinish
        }

printFinalResult ::
  ( Exception e,
    HasLogging env (Region m),
    MonadIO m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m,
    MonadTime m
  ) =>
  TimeSpec ->
  Either e b ->
  m ()
printFinalResult totalTime result = withRegion Linear $ \r -> do
  Utils.whenLeft result $ \ex ->
    let errMsg =
          mconcat
            [ "Encountered an exception. This is likely not an error in any ",
              "of the commands run but rather an error in Shrun itself: ",
              T.pack (displayException ex)
            ]
        fatalLog =
          MkLog
            { cmd = Nothing,
              msg = errMsg,
              lvl = LevelFatal,
              mode = LogModeFinish
            }
     in Logging.putRegionLog r fatalLog

  let totalTimeTxt = formatRelativeTime (Utils.timeSpecToRelTime totalTime)
      finalLog =
        MkLog
          { cmd = Nothing,
            msg = T.pack totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  Logging.putRegionLog r finalLog

counter ::
  ( HasCommands env,
    HasLogging env (Region m),
    HasTimeout env,
    MonadIORef m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m,
    MonadThread m,
    MonadTVar m,
    MonadTime m
  ) =>
  m ()
counter = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  microsleep 100_000
  withRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- newIORef 0
    Utils.whileM_ (keepRunning r timer timeout) $ do
      elapsed <- do
        sleep 1
        modifyIORef' timer (+ 1)
        readIORef timer
      logCounter r elapsed

logCounter ::
  ( HasLogging env (Region m),
    MonadReader env m,
    MonadTBQueue m
  ) =>
  Region m ->
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
  Logging.regionLogToConsoleQueue region logging lg

keepRunning ::
  forall m env.
  ( HasCommands env,
    HasLogging env (Region m),
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m
  ) =>
  Region m ->
  IORef Natural ->
  Maybe Timeout ->
  m Bool
keepRunning region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      cmdDisplay <- asks (view #cmdDisplay . getLogging @env @(Region m))
      allCmds <- asks getCommands
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- readTVarM completedCmdsTVar

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LogFmt.displayCmd cmd cmdDisplay : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Logging.putRegionLog region $
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
  ( HasLogging env (Region m),
    MonadFsWriter m,
    MonadIO m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTBQueue m
  ) =>
  m void
pollQueueToConsole = do
  sem <- liftIO $ newQSem 0
  queue <- asks (view #consoleLogging . getLogging)
  forever $ do
    -- NOTE: Applying the same semaphore logic from pollQueueToFile here.
    liftIO $ waitQSem sem
    tryReadTBQueueM queue >>= traverse_ printConsoleLog
    liftIO $ signalQSem sem

printConsoleLog :: MonadRegionLogger m => LogRegion (Region m) -> m ()
printConsoleLog (LogNoRegion consoleLog) = logGlobal (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logRegion m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  ( MonadFsWriter m,
    MonadIO m,
    MonadTBQueue m
  ) =>
  FileLogging ->
  m void
pollQueueToFile fileLogging = do
  sem <- liftIO $ newQSem 0
  forever $ do
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    --
    -- Testing shows that the QSem here appears to work as advertized:
    -- Without it, the final "Finished" log rarely shows up in the file logs.
    -- With it, it consistently shows up.
    liftIO $ waitQSem sem
    tryReadTBQueueM queue >>= traverse_ (logFile h)
    liftIO $ signalQSem sem
  where
    (h, queue) = fileLogging ^. #log

logFile :: MonadFsWriter m => Handle -> FileLog -> m ()
logFile h = hPutUtf8 h . view #unFileLog
