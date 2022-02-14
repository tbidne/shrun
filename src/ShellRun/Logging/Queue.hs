-- | Provides the 'LogTextQueue' type and associated functions. This is intended
-- to be used to provide a command "log queue", e.g., for concurrency.
module ShellRun.Logging.Queue
  ( -- * LogText
    LogText (MkLogText, unLogText),
    logToText,

    -- * Queue
    LogTextQueue (..),
    readQueue,
    writeQueue,
    flushQueue,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Data.Time.Clock qualified as Clock
import ShellRun.Logging.Log (Log)
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude

-- | 'LogText' is a textual representation of a given 'Log'. No coloring
-- is included, but we include the prefix (e.g. Warn) along with a timestamp.
newtype LogText = UnsafeLogText {unLogText :: Text}
  deriving (Eq, Show)

pattern MkLogText :: Text -> LogText
pattern MkLogText t <- UnsafeLogText t

{-# COMPLETE MkLogText #-}

-- | Formats a 'Log' into a 'LogText'. Applies prefix and timestamp.
logToText :: Log -> IO LogText
logToText log = do
  currTime <- Clock.getCurrentTime
  let logFormatted = Log.formatLogNoColor log
      formatted = "[" <> showt currTime <> "] " <> logFormatted <> "\n"
  pure $ UnsafeLogText formatted

-- | Newtype wrapper over a 'TBQueue'.
newtype LogTextQueue = MkLogTextQueue {getLogTextQueue :: TBQueue LogText}

instance Show LogTextQueue where
  show _ = "<MkLogTextQueue>"

-- | Atomically writes to the queue.
writeQueue :: MonadIO m => LogTextQueue -> Log -> m ()
writeQueue queue = liftIO . (writeq <=< logToText)
  where
    writeq = STM.atomically . TBQueue.writeTBQueue (getLogTextQueue queue)

-- | Atomically reads from the queue. Does not retry.
readQueue :: MonadIO m => LogTextQueue -> m (Maybe LogText)
readQueue = liftIO . STM.atomically . TBQueue.tryReadTBQueue . getLogTextQueue

-- | Atomically flushes the queue's entire contents. Does not retry.
flushQueue :: MonadIO m => LogTextQueue -> m [LogText]
flushQueue = liftIO . STM.atomically . STM.flushTBQueue . getLogTextQueue
