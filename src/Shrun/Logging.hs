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
    regionLogToConsoleQueue,
    logToFileQueue,
  )
where

import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Env.Types
  ( HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
  )
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types
  ( FileLog,
    Log,
    LogRegion (LogRegion),
  )
import Shrun.Prelude

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionLog ::
  forall m env.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadReader env m,
    MonadSTM m,
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

  (consoleLogging, queue) <- asks (getConsoleLogging @_ @(Region m))

  let formatted = Formatting.formatConsoleLog keyHide consoleLogging lg
      regionLog = LogRegion (lg ^. #mode) region formatted

  regionLogToConsoleQueue queue regionLog
  for_ mFileLogging $ \fl -> do
    fileLog <- Formatting.formatFileLog keyHide fl lg
    logToFileQueue fl fileLog
{-# INLINEABLE putRegionLog #-}

-- | Writes the log to the console queue.
regionLogToConsoleQueue ::
  ( HasCallStack,
    MonadSTM m
  ) =>
  -- | Region.
  TBQueue (LogRegion (Region m)) ->
  -- | Log to send.
  LogRegion (Region m) ->
  m ()
regionLogToConsoleQueue = writeTBQueueA
{-# INLINEABLE regionLogToConsoleQueue #-}

-- | Writes the log to the file queue.
logToFileQueue ::
  ( HasCallStack,
    MonadSTM m
  ) =>
  -- | FileLogging config.
  FileLoggingEnv ->
  -- | Log to send.
  FileLog ->
  m ()
logToFileQueue fileLogging = writeTBQueueA (fileLogging ^. #file % #queue)
{-# INLINEABLE logToFileQueue #-}
