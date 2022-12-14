-- | This module provides convenience functions for formatting and
-- sending logs to destinations.
--
-- @since 0.1
module Shrun.Logging.Log
  ( putRegionLog,
    regionLogToConsoleQueue,
  )
where

import Effects.MonadTime (MonadTime (..))
import Shrun.Configuration.Env.Types (HasLogging (..), Logging, FileLogging)
import Shrun.Logging.Formatting qualified as LFormat
import Shrun.Logging.Types (Log (..), LogRegion (..))
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
--
-- @since 0.3
putRegionLog ::
  ( HasLogging env r,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m
  ) =>
  r ->
  Log ->
  m ()
putRegionLog region lg =
  asks getLogging >>= \logging -> do
    regionLogToConsoleQueue region logging lg
    U.whenJust (logging ^. #fileLogging) (`logToFileQueue` lg)

-- | Writes the log to the console queue.
--
-- @since 0.7
regionLogToConsoleQueue ::
  ( MonadTBQueue m
  ) =>
  r ->
  Logging r ->
  Log ->
  m ()
regionLogToConsoleQueue region logging log =
  writeTBQueueM queue (LogRegion (log ^. #mode) region formatted)
  where
    queue = logging ^. #consoleLogging
    formatted = LFormat.formatConsoleLog logging log

-- | Writes the log to the file queue.
--
-- @since 0.7
logToFileQueue ::
  ( MonadTBQueue m,
    MonadTime m
  ) =>
  FileLogging ->
  Log ->
  m ()
logToFileQueue fileLogging log = do
  formatted <- LFormat.formatFileLog fileLogging log
  writeTBQueueM (fileLogging ^. #log % _2) formatted
