{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'LogTextQueue' type and associated functions. This is intended
-- for concurrently writing logs to a file.
--
-- @since 0.1
module Shrun.Logging.Queue
  ( -- * LogText
    LogText (MkLogText),
    _LogText,
    formatFileLog,

    -- * Queue
    LogTextQueue (..),
    _MkLogTextQueue,
    readQueue,
    writeQueue,
    flushQueue,
  )
where

import Data.Text qualified as T
import Shrun.Effects.Mutable (Mutable (..))
import Shrun.Effects.Timing (Timing (..))
import Shrun.Logging.Types (Log (..))
import Shrun.Logging.Types qualified as Log
import Shrun.Prelude

-- $setup
-- >>> import Shrun.Logging.Types (LogDest (..), LogLevel (..), LogMode (..))
-- >>> import Data.Text qualified as T
-- >>> :{
--  hardcodeTimestamp :: LogText -> Text
--  hardcodeTimestamp (MkLogText txt) = ts <> dropTimestamp txt
--    where
--      dropTimestamp = T.dropWhile (/= ']')
--      ts = "[2022-02-23 20:58:04.231933782 UTC"
-- :}

-- | 'LogText' is a textual representation of a given 'Log'. No coloring
-- is included, but we include the prefix (e.g. Warn) along with a timestamp.
--
-- @since 0.1
newtype LogText = UnsafeLogText
  { -- | @since 0.1
    unLogText :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.5
_LogText :: Getter LogText Text
_LogText = to (\(MkLogText t) -> t)

-- | @since 0.1
pattern MkLogText :: Text -> LogText
pattern MkLogText t <- UnsafeLogText t

{-# COMPLETE MkLogText #-}

-- | Formats a 'Log' into a 'LogText'. Applies prefix and timestamp.
--
-- ==== __Examples__
--
-- >>> :{
--   let log = MkLog Nothing "Running time: 2 seconds" Info Set LogBoth
--    -- timestamp hardcoded for testing
--    in fmap hardcodeTimestamp (formatFileLog log)
-- :}
-- "[2022-02-23 20:58:04.231933782 UTC] [Info] Running time: 2 seconds\n"
--
-- >>> :{
--   let log = MkLog (Just "cmd") "cmd: command not found" Error Set LogBoth
--    in fmap hardcodeTimestamp (formatFileLog log)
-- :}
-- "[2022-02-23 20:58:04.231933782 UTC] [Error] [cmd] cmd: command not found\n"
--
-- @since 0.1
formatFileLog :: Timing m => Log -> m LogText
formatFileLog log = do
  currTime <- getSystemTime
  let msg' = T.strip $ log ^. #msg
      formatted = case log ^. #cmd of
        Nothing -> prefix <> msg'
        Just com -> prefix <> "[" <> (com ^. #command) <> "] " <> msg'
      withTimestamp = "[" <> showt currTime <> "] " <> formatted <> "\n"
  pure $ UnsafeLogText withTimestamp
  where
    prefix = Log.logToPrefix log
{-# INLINEABLE formatFileLog #-}

-- | Newtype wrapper over a 'TBQueue'.
--
-- @since 0.1
newtype LogTextQueue = MkLogTextQueue
  { -- | @since 0.1
    getLogTextQueue :: TBQueue LogText
  }

-- | @since 0.5
makePrisms ''LogTextQueue

-- | @since 0.1
instance Show LogTextQueue where
  show _ = "<MkLogTextQueue>"
  {-# INLINEABLE show #-}

-- | Atomically writes to the queue.
--
-- @since 0.1
writeQueue :: (Mutable m, Timing m) => LogTextQueue -> Log -> m ()
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
