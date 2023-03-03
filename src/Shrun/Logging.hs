-- | Provides logging functionality. This is a high-level picture of how
-- logging works:
--
-- 1. "Shrun.IO" sends logs per command based on the environment (i.e. is file
--    logging on and/or do we log subcommands). If any logs are produced, they
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
--
-- @since 0.7
module Shrun.Logging
  ( -- * Types

    -- ** Core
    Log (..),
    LogMode (..),
    LogLevel (..),

    -- ** Console Logs
    ConsoleLog,
    LogRegion (..),

    -- ** File Logs
    FileLog,

    -- ** Class
    MonadRegionLogger (..),

    -- * Writing logs
    putRegionLog,
    regionLogToConsoleQueue,
    logToFileQueue,

    -- * Formatting
    formatConsoleLog,
    formatFileLog,
  )
where

import Shrun.Configuration.Env.Types (FileLogging, HasLogging (..), Logging)
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Logging.Types
  ( ConsoleLog,
    FileLog,
    Log (..),
    LogLevel (..),
    LogMode (..),
    LogRegion (..),
  )
import Shrun.Prelude

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
--
-- @since 0.3
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
    regionLogToConsoleQueue region logging lg
    for_ (logging ^. #fileLogging) (`logToFileQueue` lg)

-- | Writes the log to the console queue.
--
-- @since 0.7
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
  writeTBQueueM queue (LogRegion (log ^. #mode) region formatted)
  where
    queue = logging ^. #consoleLogging
    formatted = formatConsoleLog logging log

-- | Writes the log to the file queue.
--
-- @since 0.7
logToFileQueue ::
  ( MonadSTM m,
    MonadTime m
  ) =>
  -- | FileLogging config.
  FileLogging ->
  -- | Log to send.
  Log ->
  m ()
logToFileQueue fileLogging log = do
  formatted <- formatFileLog fileLogging log
  writeTBQueueM (fileLogging ^. #log % _2) formatted
