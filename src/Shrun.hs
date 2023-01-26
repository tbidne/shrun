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

import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time.Relative (formatRelativeTime, formatSeconds)
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread as X (microsleep, sleep)
import Effects.Time (TimeSpec, withTiming)
import Shrun.Configuration.Env.Types
  ( FileLogging,
    HasAnyError (..),
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

-- | Entry point
--
-- @since 0.1
shrun ::
  ( HasAnyError env,
    HasCommands env,
    HasLogging env (Region m),
    HasTimeout env,
    MonadAsync m,
    MonadExit m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadProcess m,
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
      (logging ^. #fileLogging)

    -- cancel consoleLogger, print remaining logs
    Async.cancel consoleLogger
    let consoleQueue = logging ^. #consoleLogging
    flushTBQueueM consoleQueue >>= traverse_ printConsoleLog

    -- if any processes have failed, exit with an error
    anyErrorRef <- asks getAnyError
    anyError <- readTVarM anyErrorRef
    when anyError exitFailure
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
  ( HasAnyError env,
    HasCommands env,
    HasLogging env (Region m),
    MonadHandleReader m,
    MonadIORef m,
    MonadMask m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
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
    HasAnyError env,
    HasLogging env (Region m),
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
              T.pack (displayException ex)
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
    anyError <- asks getAnyError
    writeTVarM anyError True

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
      cmdDisplay <- asks (view #cmdDisplay . getLogging @env @(Region m))
      allCmds <- asks getCommands
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- readTVarM completedCmdsTVar

      -- update anyError
      anyError <- asks getAnyError
      writeTVarM anyError True

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
    MonadMask m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m
  ) =>
  m void
pollQueueToConsole = do
  queue <- asks (view #consoleLogging . getLogging)
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ atomicReadWrite queue printConsoleLog

printConsoleLog :: MonadRegionLogger m => LogRegion (Region m) -> m ()
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
  forever $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    atomicReadWrite queue (logFile h)
  where
    (h, queue) = fileLogging ^. #log

logFile :: MonadHandleWriter m => Handle -> FileLog -> m ()
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
  mask $ \restore -> restore (readTBQueueM queue) >>= void . logAction
