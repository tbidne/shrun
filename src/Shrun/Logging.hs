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

import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    FileLogging,
    HasLogging (..),
    Logging,
  )
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.Types
  ( Log (..),
    LogRegion (..),
  )
import Shrun.Prelude

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionLog ::
  ( HasLogging env r,
    MonadReader env m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Region.
  r ->
  -- | Log to send.
  Log ->
  m ()
putRegionLog region lg =
  asks getLogging >>= \logging -> do
    let keyHide = logging ^. #keyHide
    regionLogToConsoleQueue region logging lg
    for_ (logging ^. #fileLog) (\fl -> logToFileQueue keyHide fl lg)

-- | Writes the log to the console queue.
regionLogToConsoleQueue ::
  ( MonadSTM m
  ) =>
  -- | Region.
  r ->
  -- | Logging config.
  Logging r ->
  -- | Log to send.
  Log ->
  m ()
regionLogToConsoleQueue region logging log =
  writeTBQueueA queue (LogRegion (log ^. #mode) region formatted)
  where
    queue = logging ^. #consoleLog
    formatted = formatConsoleLog logging log

-- | Writes the log to the file queue.
logToFileQueue ::
  ( MonadSTM m,
    MonadTime m
  ) =>
  -- | How to display the command.
  CmdDisplay ->
  -- | FileLogging config.
  FileLogging ->
  -- | Log to send.
  Log ->
  m ()
logToFileQueue cmdDisplay fileLogging log = do
  formatted <- formatFileLog cmdDisplay fileLogging log
  writeTBQueueA (fileLogging ^. #log % _2) formatted
