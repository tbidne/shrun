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
import Shrun.Configuration.Env.Types (HasLogging (..), Logging)
import Shrun.Logging.Formatting qualified as LFormat
import Shrun.Logging.Types (Log (..), LogRegion (..))
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Conditionally writes a log to the console region and file, depending on
-- the 'HasLogging' environment.
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
    logToFileQueue logging lg
    regionLogToConsoleQueue region logging lg
{-# INLINEABLE putRegionLog #-}

-- | @maybePrintLog fn log@ applies @fn@ if the @log@ does __not__ have dest
-- 'LogDestFile'. Otherwise does nothing.
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

-- | Sends the log to the file queue as long as the dest is not 'LogDestConsole'.
--
-- @since 0.7
logToFileQueue ::
  ( MonadTBQueue m,
    MonadTime m
  ) =>
  Logging r ->
  Log ->
  m ()
logToFileQueue logging log =
  U.whenJust (logging ^. #fileLogging) $ \fl -> do
    formatted <- LFormat.formatFileLog fl log
    writeTBQueueM (fl ^. #log % _2) formatted
{-# INLINEABLE logToFileQueue #-}
