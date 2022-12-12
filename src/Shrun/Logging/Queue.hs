{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'LogTextQueue' type and associated functions. This is intended
-- for concurrently writing logs to a file.
--
-- @since 0.1
module Shrun.Logging.Queue
  ( -- * LogText
    LogText (MkLogText),
    formatFileLog,

    -- * Queue
    LogTextQueue (..),
    readQueue,
    writeQueue,
    flushQueue,
  )
where

import Effects.MonadSTM (MonadTBQueue (..))
import Effects.MonadTime (MonadTime (..), formatLocalTime)
import Shrun.Configuration.Env.Types (HasLogging (..))
import Shrun.Logging.Formatting (stripChars')
import Shrun.Logging.Types
  ( Log (..),
    LogText (..),
    LogTextQueue (..),
  )
import Shrun.Logging.Types qualified as Log
import Shrun.Prelude

-- | @since 0.5
_LogText :: Getter LogText Text
_LogText = to (\(MkLogText t) -> t)

-- | Formats a 'Log' into a 'LogText'. Applies prefix and timestamp.
--
-- @since 0.1
formatFileLog :: (HasLogging env, MonadReader env m, MonadTime m) => Log -> m LogText
formatFileLog log = do
  currTime <- formatLocalTime <$> getSystemTime
  stripControl <- asks getFileLogStripControl
  let msg' = " " <> stripChars' (log ^. #msg) stripControl
      formatted = case log ^. #cmd of
        Nothing -> prefix <> msg'
        Just com -> prefix <> "[" <> (com ^. #command) <> "]" <> msg'
      withTimestamp = "[" <> pack currTime <> "]" <> formatted <> "\n"
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
    MonadTBQueue m,
    MonadTime m
  ) =>
  LogTextQueue ->
  Log ->
  m ()
writeQueue queue = writeq <=< formatFileLog
  where
    writeq = writeTBQueueM (queue ^. #getLogTextQueue)
{-# INLINEABLE writeQueue #-}

-- | Atomically reads from the queue. Does not retry.
--
-- @since 0.1
readQueue :: MonadTBQueue m => LogTextQueue -> m (Maybe LogText)
readQueue = tryReadTBQueueM . view #getLogTextQueue
{-# INLINEABLE readQueue #-}

-- | Atomically flushes the queue's entire contents. Does not retry.
--
-- @since 0.1
flushQueue :: MonadTBQueue m => LogTextQueue -> m (List LogText)
flushQueue = flushTBQueueM . view #getLogTextQueue
{-# INLINEABLE flushQueue #-}
