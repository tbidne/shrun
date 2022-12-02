{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'LogTextQueue' type and associated functions. This is intended
-- for concurrently writing logs to a file.
--
-- @since 0.1
module Shrun.Logging.Queue
  ( -- * LogText
    LogText (MkLogText),
    _MkLogText,
    formatFileLog,

    -- * Queue
    LogTextQueue (..),
    _MkLogTextQueue,
    readQueue,
    writeQueue,
    flushQueue,
  )
where

import Effects.MonadTime (MonadTime (..), formatLocalTime)
import Shrun.Configuration.Env.Types (HasLogging (..))
import Shrun.Effects.Mutable (Mutable (..))
import Shrun.Logging.Formatting (stripChars')
import Shrun.Logging.Types
  ( Log (..),
    LogText (..),
    LogTextQueue (..),
    _MkLogText,
    _MkLogTextQueue,
  )
import Shrun.Logging.Types qualified as Log
import Shrun.Prelude

-- | @since 0.5
_LogText :: Getter LogText Text
_LogText = to (\(MkLogText t) -> t)

-- | Formats a 'Log' into a 'LogText'. Applies prefix and timestamp.
--
-- ==== __Examples__
--
-- @
-- formatFileLog $ MkLog Nothing "Running time: 2 seconds" Info Set LogBoth
-- "[2022-02-23 20:58:04.231933782 UTC] [Info] Running time: 2 seconds\n"
-- @
--
-- @
-- formatFileLog $ MkLog (Just "cmd") "cmd: command not found" Error Set LogBoth
-- "[2022-02-23 20:58:04.231933782 UTC] [Error] [cmd] cmd: command not found\n"
-- @
--
-- @since 0.1
formatFileLog :: (HasLogging env, MonadReader env m, MonadTime m) => Log -> m LogText
formatFileLog log = do
  currTime <- formatLocalTime <$> getSystemTime
  stripControl <- asks getFileLogStripControl
  let msg' = stripChars' (log ^. #msg) stripControl
      formatted = case log ^. #cmd of
        Nothing -> prefix <> msg'
        Just com -> prefix <> "[" <> (com ^. #command) <> "] " <> msg'
      withTimestamp = "[" <> pack currTime <> "] " <> formatted <> "\n"
  pure $ UnsafeLogText withTimestamp
  where
    prefix = Log.logToPrefix log
{-# INLINEABLE formatFileLog #-}

-- | Atomically writes to the queue.
--
-- @since 0.1
writeQueue ::
  ( HasLogging env,
    MonadReader env m,
    Mutable m,
    MonadTime m
  ) =>
  LogTextQueue ->
  Log ->
  m ()
writeQueue queue = writeq <=< formatFileLog
  where
    writeq = liftSTM . writeTBQueue (queue ^. _MkLogTextQueue)
{-# INLINEABLE writeQueue #-}

-- | Atomically reads from the queue. Does not retry.
--
-- @since 0.1
readQueue :: Mutable m => LogTextQueue -> m (Maybe LogText)
readQueue = liftSTM . tryReadTBQueue . view _MkLogTextQueue
{-# INLINEABLE readQueue #-}

-- | Atomically flushes the queue's entire contents. Does not retry.
--
-- @since 0.1
flushQueue :: Mutable m => LogTextQueue -> m (List LogText)
flushQueue = liftSTM . flushTBQueue . view _MkLogTextQueue
{-# INLINEABLE flushQueue #-}
