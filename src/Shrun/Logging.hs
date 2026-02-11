-- | Provides logging functionality. This is a high-level picture of how
-- logging works:
--
-- 1. "Shrun.IO" sends logs per command based on the environment (i.e. is file
--    logging on and/or do we log commands). If any logs are produced, they
--    are formatted and sent directly to a queue.
--
-- 2. "Shrun" also produces logs. These are "higher-level" e.g. success/failure
--    status of a given command, fatal errors, etc. "Shrun" uses the functions
--    here (e.g. putRegionLog) that handles deciding if a given log
--    should be written to either/both of the console/file log queues.
--
-- 3. "Shrun" has two threads -- one for each queue -- that poll their
--    respective queues and writes logs as they are found. These do no
--    environment checking; any logs that make it to the queue are eventually
--    written.
module Shrun.Logging
  ( -- * Writing logs
    putRegionLog,
    putRegionMultiLineLog,
    regionLogToConsoleQueue,
    logToFileQueue,

    -- * Direct logs
    putRegionLogDirect,
    putRegionMultiLineLogDirect,

    -- * Debug
    putDebugLog,
    putDebugLogDirect,

    -- * Misc
    mkUnfinishedCmdLogs,
    logDebug,
    logFile,
  )
where

import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Shrun.Command.Types
  ( CommandOrd (MkCommandOrd),
    CommandPhase (CommandPhase1),
    CommandStatus
      ( CommandFailure,
        CommandRunning,
        CommandSuccess,
        CommandWaiting
      ),
  )
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Env.Types
  ( HasCommands,
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasLogging,
    getReadCommandStatus,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.MonadRegionLogger qualified as MRL
import Shrun.Logging.Types
  ( FileLog,
    Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelDebug, LevelWarn),
    LogMessage (UnsafeLogMessage),
    LogMode (LogModeFinish),
    LogRegion (LogRegion),
  )
import Shrun.Prelude

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionLog ::
  forall m env.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadReader env m,
    MonadTime m
  ) =>
  -- | Region.
  Region m ->
  -- | Log to send.
  Log ->
  m ()
putRegionLog region lg = do
  commonLogging <- asks getCommonLogging
  mFileLogging <- asks getFileLogging

  let keyHide = commonLogging ^. #keyHide

  (consoleLogging, queue, _) <- asks (getConsoleLogging @_ @(Region m))

  formatted <- Formatting.formatConsoleLog keyHide consoleLogging lg
  let regionLog = LogRegion (lg ^. #mode) region formatted

  regionLogToConsoleQueue queue regionLog
  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileLog keyHide fl lg
    logToFileQueue fl fileLog
{-# INLINEABLE putRegionLog #-}

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionMultiLineLog ::
  forall m env.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadReader env m,
    MonadTime m
  ) =>
  -- | Region.
  Region m ->
  -- | Log to send.
  NonEmpty Log ->
  m ()
putRegionMultiLineLog region logs = do
  commonLogging <- asks getCommonLogging
  mFileLogging <- asks getFileLogging

  let keyHide = commonLogging ^. #keyHide

  (consoleLogging, queue, _) <- asks (getConsoleLogging @_ @(Region m))

  formatted <- Formatting.formatConsoleMultiLineLogs keyHide consoleLogging logs
  let regionLog = LogRegion mode region formatted

  regionLogToConsoleQueue queue regionLog
  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileMultiLineLogs keyHide fl logs
    logToFileQueue fl fileLog
  where
    mode = NE.head logs ^. #mode
{-# INLINEABLE putRegionMultiLineLog #-}

-- | Writes the log to the console queue.
regionLogToConsoleQueue ::
  ( HasCallStack,
    MonadAtomic m
  ) =>
  -- | Region.
  TBQueue (LogRegion (Region m)) ->
  -- | Log to send.
  LogRegion (Region m) ->
  m ()
regionLogToConsoleQueue = writeTBQueueA'
{-# INLINEABLE regionLogToConsoleQueue #-}

-- | Writes the log to the file queue.
logToFileQueue ::
  ( HasCallStack,
    MonadAtomic m
  ) =>
  -- | FileLogging config.
  FileLoggingEnv ->
  -- | Log to send.
  FileLog ->
  m ()
logToFileQueue fileLogging = writeTBQueueA' (fileLogging ^. #file % #queue)
{-# INLINEABLE logToFileQueue #-}

-- | Returns formatted log for unfinished commands (waiting and running).
-- Does not actually cancel any commands itself; that is handled by
-- async (race_).
--
-- Returns "multi logs", as each command is rendered on a newline.
-- Hence this should be used with the multi-line log options.
mkUnfinishedCmdLogs ::
  forall m env.
  ( HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    MonadAtomic m,
    MonadReader env m
  ) =>
  m (Tuple2 (Maybe (NonEmpty Log)) (Maybe (NonEmpty Log)))
mkUnfinishedCmdLogs = do
  keyHide <- asks (view #keyHide . getCommonLogging)

  -- Statuses receive no updates at this point (command threads have finished
  -- or been killed), so this should be safe.
  commandsStatus <- getReadCommandStatus <&> view #unCommandStatusMap

  let (waiting, running) = foldl' go (Set.empty, Set.empty) commandsStatus
      go acc@(ws, rs) (cmd, status) = case status of
        CommandSuccess -> acc
        CommandFailure -> acc
        CommandRunning _ -> (ws, Set.insert (MkCommandOrd cmd) rs)
        CommandWaiting -> (Set.insert (MkCommandOrd cmd) ws, rs)

      cmdToTxt :: CommandOrd CommandPhase1 -> Text
      cmdToTxt cmd =
        "- " <> Formatting.displayCmd (cmd ^. #unCommandOrd) keyHide ^. #unUnlinedText

      mkLog :: Text -> Log
      mkLog txt =
        MkLog
          { cmd = Nothing,
            msg = UnsafeLogMessage txt,
            lvl = LevelWarn,
            mode = LogModeFinish
          }

      mkLogs :: UnlinedText -> HashSet (CommandOrd CommandPhase1) -> Maybe (NonEmpty Log)
      mkLogs pfx st =
        if Set.null st
          then Nothing
          else
            Just
              $ mkLog (pfx ^. #unUnlinedText)
              -- Final sort by CommandOrd since our intermediate structure is
              -- a HashSet.
              :| (mkLog . cmdToTxt <$> L.sort (toList st))

  pure (mkLogs waitingPrefix waiting, mkLogs runningPrefix running)
  where
    waitingPrefix = "Commands not started:"
    runningPrefix = "Attempting to cancel:"
{-# INLINEABLE mkUnfinishedCmdLogs #-}

-- | Logs to a file. This function is /not/ thread-safe! Hence care must be
-- taken to avoid it being called by multiple threads.
logFile :: (HasCallStack, MonadHandleWriter m) => Handle -> FileLog -> m ()
logFile h = (\t -> hPutUtf8 h t *> hFlush h) . view #unFileLog
{-# INLINEABLE logFile #-}

-- | Like 'putRegionLog', except this logs directly to the console / file,
-- rather than placing the log in a queue. This is for when log queues are
-- shutdown (e.g. terminated). This should only be called from a single thread.
putRegionLogDirect ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadHandleWriter m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  Log ->
  m ()
putRegionLogDirect log = do
  commonLogging <- asks getCommonLogging
  (consoleLogging, _, _) <- asks (getConsoleLogging @env @(Region m))
  mFileLogging <- asks getFileLogging

  let keyHide = view #keyHide commonLogging
  consoleLog <- Formatting.formatConsoleLog keyHide consoleLogging log

  MRL.withRegion Linear $ \r -> MRL.logRegion (log ^. #mode) r (consoleLog ^. #unConsoleLog)

  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileLog keyHide fl log
    logFile (fl ^. #file % #handle) fileLog
{-# INLINEABLE putRegionLogDirect #-}

-- | Like 'putRegionMultiLineLog', except this logs directly to the
-- console / file, rather than placing the log in a queue. This is for when
-- log queues are shutdown (e.g. terminated). This should only be called from
-- a single thread.
putRegionMultiLineLogDirect ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadHandleWriter m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  NonEmpty Log ->
  m ()
putRegionMultiLineLogDirect logs@(log :| _) = do
  commonLogging <- asks getCommonLogging
  (consoleLogging, _, _) <- asks (getConsoleLogging @env @(Region m))
  mFileLogging <- asks getFileLogging

  let keyHide = view #keyHide commonLogging
  consoleLog <- Formatting.formatConsoleMultiLineLogs keyHide consoleLogging logs

  MRL.withRegion Linear $ \r -> MRL.logRegion (log ^. #mode) r (consoleLog ^. #unConsoleLog)

  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileMultiLineLogs keyHide fl logs
    logFile (fl ^. #file % #handle) fileLog
{-# INLINEABLE putRegionMultiLineLogDirect #-}

putDebugLogDirect ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadHandleWriter m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  LogMessage ->
  m ()
putDebugLogDirect = putDebugLogHelper putRegionLogDirect
{-# INLINEABLE putDebugLogDirect #-}

putDebugLog ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  LogMessage ->
  m ()
putDebugLog = putDebugLogHelper (\log -> MRL.withRegion Linear $ \r -> putRegionLog r log)
{-# INLINEABLE putDebugLog #-}

putDebugLogHelper ::
  ( HasCommonLogging env,
    MonadReader env m
  ) =>
  (Log -> m ()) ->
  LogMessage ->
  m ()
putDebugLogHelper logFn msg = do
  logDebug $ \lvl -> do
    let log =
          MkLog
            { cmd = Nothing,
              msg,
              lvl,
              mode = LogModeFinish
            }
    logFn log
{-# INLINEABLE putDebugLogHelper #-}

-- | Rungs the action when debug is on.
logDebug ::
  ( HasCommonLogging env,
    MonadReader env m
  ) =>
  (LogLevel -> m ()) ->
  m ()
logDebug logFn = do
  debug <- asks (view (#debug % #unDebug) . getCommonLogging)
  when debug (logFn LevelDebug)
{-# INLINEABLE logDebug #-}
