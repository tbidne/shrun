{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Shrun.Logging.RegionLogger (RegionLogger)
import Shrun.Logging.RegionLogger qualified as MRL
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
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    Reader env :> es,
    Time :> es
  ) =>
  -- | Region.
  r ->
  -- | Log to send.
  Log ->
  Eff es ()
putRegionLog region lg = do
  commonLogging <- asks @env getCommonLogging
  mFileLogging <- asks @env getFileLogging

  let cmdIndex = view #commandIndex commonLogging
      keyHide = commonLogging ^. #keyHide

  (consoleLogging, queue, _) <- asks @env (getConsoleLogging @_ @r)

  formatted <- Formatting.formatConsoleLog @env cmdIndex keyHide consoleLogging lg
  let regionLog = LogRegion (lg ^. #mode) region formatted

  regionLogToConsoleQueue queue regionLog
  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileLog @env cmdIndex keyHide fl lg
    logToFileQueue fl fileLog

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionMultiLineLog ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    Reader env :> es,
    Time :> es
  ) =>
  -- | Region.
  r ->
  -- | Log to send.
  NonEmpty Log ->
  Eff es ()
putRegionMultiLineLog region logs = do
  commonLogging <- asks @env getCommonLogging
  mFileLogging <- asks @env getFileLogging

  let cmdIndex = view #commandIndex commonLogging
      keyHide = commonLogging ^. #keyHide

  (consoleLogging, queue, _) <- asks @env (getConsoleLogging @_ @r)

  formatted <- Formatting.formatConsoleMultiLineLogs @env cmdIndex keyHide consoleLogging logs
  let regionLog = LogRegion mode region formatted

  regionLogToConsoleQueue queue regionLog
  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileMultiLineLogs @env cmdIndex keyHide fl logs
    logToFileQueue fl fileLog
  where
    mode = NE.head logs ^. #mode

-- | Writes the log to the console queue.
regionLogToConsoleQueue ::
  (Concurrent :> es) =>
  -- | Region.
  TBQueue (LogRegion r) ->
  -- | Log to send.
  LogRegion r ->
  Eff es ()
regionLogToConsoleQueue = writeTBQueueA'

-- | Writes the log to the file queue.
logToFileQueue ::
  (Concurrent :> es) =>
  -- | FileLogging config.
  FileLoggingEnv ->
  -- | Log to send.
  FileLog ->
  Eff es ()
logToFileQueue fileLogging = writeTBQueueA' (fileLogging ^. #file % #queue)

-- | Returns formatted log for unfinished commands (waiting and running).
-- Does not actually cancel any commands itself; that is handled by
-- async (race_).
--
-- Returns "multi logs", as each command is rendered on a newline.
-- Hence this should be used with the multi-line log options.
mkUnfinishedCmdLogs ::
  forall env es.
  ( HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    Concurrent :> es,
    Reader env :> es
  ) =>
  Eff es (Tuple2 (Maybe (NonEmpty Log)) (Maybe (NonEmpty Log)))
mkUnfinishedCmdLogs = do
  commonLogging <- asks @env getCommonLogging

  let cmdIndex = view #commandIndex commonLogging
      keyHide = view #keyHide commonLogging

  -- Statuses receive no updates at this point (command threads have finished
  -- or been killed), so this should be safe.
  commandsStatus <- getReadCommandStatus @env <&> view #unCommandStatusMap

  let (waiting, running) = foldl' go (Set.empty, Set.empty) commandsStatus
      go acc@(ws, rs) (cmd, status) = case status of
        CommandSuccess -> acc
        CommandFailure -> acc
        CommandRunning _ -> (ws, Set.insert (MkCommandOrd cmd) rs)
        CommandWaiting -> (Set.insert (MkCommandOrd cmd) ws, rs)

      cmdToTxt :: CommandOrd CommandPhase1 -> Text
      cmdToTxt cmd =
        "- " <> Formatting.displayCmd (cmd ^. #unCommandOrd) cmdIndex keyHide ^. #unUnlinedText

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

-- | Logs to a file. This function is /not/ thread-safe! Hence care must be
-- taken to avoid it being called by multiple threads.
logFile ::
  ( CanWrite p,
    HasCallStack,
    HandleWriter :> es
  ) =>
  LockedHandle p ->
  FileLog ->
  Eff es ()
logFile lh = liftLocked (\h t -> hPutUtf8 h t *> hFlush h) lh . view #unFileLog

-- | Like 'putRegionLog', except this logs directly to the console / file,
-- rather than placing the log in a queue. This is for when log queues are
-- shutdown (e.g. terminated). This should only be called from a single thread.
putRegionLogDirect ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Log ->
  Eff es ()
putRegionLogDirect log = do
  commonLogging <- asks @env getCommonLogging
  (consoleLogging, _, _) <- asks (getConsoleLogging @env @r)
  mFileLogging <- asks @env getFileLogging

  let cmdIndex = view #commandIndex commonLogging
      keyHide = view #keyHide commonLogging
  consoleLog <- Formatting.formatConsoleLog @env cmdIndex keyHide consoleLogging log

  MRL.withRegion @r Linear $ \r -> MRL.logRegion (log ^. #mode) r (consoleLog ^. #unConsoleLog)

  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileLog @env cmdIndex keyHide fl log
    logFile (fl ^. #file % #handle) fileLog

-- | Like 'putRegionMultiLineLog', except this logs directly to the
-- console / file, rather than placing the log in a queue. This is for when
-- log queues are shutdown (e.g. terminated). This should only be called from
-- a single thread.
putRegionMultiLineLogDirect ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  NonEmpty Log ->
  Eff es ()
putRegionMultiLineLogDirect logs@(log :| _) = do
  commonLogging <- asks @env getCommonLogging
  (consoleLogging, _, _) <- asks (getConsoleLogging @env @r)
  mFileLogging <- asks @env getFileLogging

  let cmdIndex = view #commandIndex commonLogging
      keyHide = view #keyHide commonLogging
  consoleLog <- Formatting.formatConsoleMultiLineLogs @env cmdIndex keyHide consoleLogging logs

  MRL.withRegion @r Linear $ \r -> MRL.logRegion (log ^. #mode) r (consoleLog ^. #unConsoleLog)

  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileMultiLineLogs @env cmdIndex keyHide fl logs
    logFile (fl ^. #file % #handle) fileLog

putDebugLogDirect ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  LogMessage ->
  Eff es ()
putDebugLogDirect = putDebugLogHelper @env (putRegionLogDirect @env @r)

putDebugLog ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  LogMessage ->
  Eff es ()
putDebugLog = putDebugLogHelper @env (\log -> MRL.withRegion @r Linear $ \r -> putRegionLog @env @r r log)

putDebugLogHelper ::
  forall env es.
  ( HasCommonLogging env,
    Reader env :> es
  ) =>
  (Log -> Eff es ()) ->
  LogMessage ->
  Eff es ()
putDebugLogHelper logFn msg = do
  logDebug @env $ \lvl -> do
    let log =
          MkLog
            { cmd = Nothing,
              msg,
              lvl,
              mode = LogModeFinish
            }
    logFn log

-- | Rungs the action when debug is on.
logDebug ::
  forall env es.
  ( HasCommonLogging env,
    Reader env :> es
  ) =>
  (LogLevel -> Eff es ()) ->
  Eff es ()
logDebug logFn = do
  debug <- asks @env (view (#debug % #unDebug) . getCommonLogging)
  when debug (logFn LevelDebug)
