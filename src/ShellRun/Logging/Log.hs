-- | This module provides convenience functions for formatting and
-- sending logs to destinations.
--
-- @since 0.1
module ShellRun.Logging.Log
  ( -- * High-level
    putLog,
    putRegionLog,

    -- * Low-level
    maybePrintLog,
    maybeSendLogToQueue,
  )
where

import ShellRun.Env.Types (HasLogging (..))
import ShellRun.Logging.Formatting qualified as LFormat
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Logging.Types (Log (..), LogDest (..))
import ShellRun.Prelude

-- | Conditionally writes a log to the console and file, depending on
-- the 'HasLogging' environment.
--
-- @since 0.3
putLog ::
  ( HasLogging env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m
  ) =>
  Log ->
  m ()
putLog log = do
  b <- asks getGlobalLogging
  if b
    then do
      maybeSendLogToQueue log
      maybePrintLog logFn log
    else pure ()

-- | Conditionally writes a log to the console region and file, depending on
-- the 'HasLogging' environment.
--
-- @since 0.3
putRegionLog ::
  ( HasLogging env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m
  ) =>
  Region m ->
  Log ->
  m ()
putRegionLog region lg = do
  b <- asks getGlobalLogging
  if b
    then do
      let logRegionFn = logModeToRegionFn $ lg ^. #mode
      maybeSendLogToQueue lg
      maybePrintLog (logRegionFn region) lg
    else pure ()
{-# INLINEABLE putRegionLog #-}

-- | @maybePrintLog fn log@ applies @fn@ if the @log@ has dest 'LogFile'.
-- Otherwise does nothing.
--
-- @since 0.3
maybePrintLog ::
  ( HasLogging env,
    MonadReader env m
  ) =>
  (Text -> m ()) ->
  Log ->
  m ()
maybePrintLog fn log =
  case log ^. #dest of
    LogFile -> pure ()
    _ -> LFormat.formatConsoleLog log >>= fn
{-# INLINEABLE maybePrintLog #-}

-- | Sends the log to the file queue as long as the dest is not 'LogConsole'.
--
-- @since 0.3
maybeSendLogToQueue ::
  ( HasLogging env,
    MonadIO m,
    MonadReader env m
  ) =>
  Log ->
  m ()
maybeSendLogToQueue log =
  case log ^. #dest of
    LogConsole -> pure ()
    _ -> do
      fileLogging <- asks getFileLogging
      case fileLogging of
        Nothing -> pure ()
        Just (_, queue) -> do
          Queue.writeQueue queue log
{-# INLINEABLE maybeSendLogToQueue #-}
