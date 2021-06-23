{-# LANGUAGE ImportQualifiedPost #-}

-- | Provides the 'LogQueue' type and associated functions. This is intended
-- to be used to provide a command "log queue", e.g., for concurrency.
module ShellRun.Logging.Queue
  ( -- * Queue type
    LogQueue (..),

    -- * Queue functions
    readQueue,
    writeQueue,
    flushQueue,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as MIO
import ShellRun.Logging.Log (Log)

-- | Newtype wrapper over a 'TBQueue'.
newtype LogQueue = MkLogQueue {getLogQueue :: TBQueue Log}

-- | Atomically writes to the queue.
writeQueue :: MonadIO m => LogQueue -> Log -> m ()
writeQueue queue = MIO.liftIO . STM.atomically . TBQueue.writeTBQueue (getLogQueue queue)

-- | Atomically reads from the queue. Does not retry.
readQueue :: MonadIO m => LogQueue -> m (Maybe Log)
readQueue = MIO.liftIO . STM.atomically . TBQueue.tryReadTBQueue . getLogQueue

-- | Atomically flushes the queue's entire contents. Does not retry.
flushQueue :: MonadIO m => LogQueue -> m [Log]
flushQueue = MIO.liftIO . STM.atomically . STM.flushTBQueue . getLogQueue