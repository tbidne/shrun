{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'LogTextQueue' type and associated functions. This is intended
-- for concurrently writing logs to a file.
--
-- @since 0.1.0.0
module ShellRun.Logging.Queue
  ( -- * LogText
    LogText (MkLogText, unLogText),
    formatFileLog,

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
import ShellRun.Class.MonadTime (MonadTime (..))
import ShellRun.Logging.Log (Log (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude

-- $setup
-- >>> import ShellRun.Logging.Log (LogDest (..), LogLevel (..), LogMode (..))
-- >>> import Text.Megaparsec (Parsec)
-- >>> import Text.Megaparsec qualified as MP
-- >>> import Text.Megaparsec.Char qualified as MPC
-- >>> import Data.Text qualified as T
-- >>> :{
--  hardcodeTimestamp :: LogText -> Text
--  hardcodeTimestamp (MkLogText txt) = case MP.parse parseLog "" txt of
--    Left _ -> "<parse error>"
--    Right txt' -> txt'
--    where
--      parseLog :: Parsec Void Text Text
--      parseLog = do
--        MPC.char '['
--        MP.takeWhile1P Nothing (/= ']')
--        MPC.string "] "
--        rest <- MP.takeWhile1P Nothing (/= '\n')
--        MPC.eol
--        pure $ ts <> rest <> "\n"
--      ts = "[2022-02-23 20:58:04.231933782 UTC] "
-- :}

-- | 'LogText' is a textual representation of a given 'Log'. No coloring
-- is included, but we include the prefix (e.g. Warn) along with a timestamp.
--
-- @since 0.1.0.0
newtype LogText = UnsafeLogText
  { -- | @since 0.1.0.0
    unLogText :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

makeFieldLabelsNoPrefix ''LogText

-- | @since 0.1.0.0
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
-- @since 0.1.0.0
formatFileLog :: MonadTime m => Log -> m LogText
formatFileLog log@MkLog {cmd, msg} = do
  currTime <- getSystemTime
  let formatted = case cmd of
        Nothing -> prefix <> msg
        Just com -> prefix <> "[" <> (com ^. #command) <> "] " <> msg
      withTimestamp = "[" <> showt currTime <> "] " <> formatted <> "\n"
  pure $ UnsafeLogText withTimestamp
  where
    prefix = Log.logToPrefix log

-- | Newtype wrapper over a 'TBQueue'.
--
-- @since 0.1.0.0
newtype LogTextQueue = MkLogTextQueue
  { -- | @since 0.1.0.0
    getLogTextQueue :: TBQueue LogText
  }

makeFieldLabelsNoPrefix ''LogTextQueue

-- | @since 0.1.0.0
instance Show LogTextQueue where
  show _ = "<MkLogTextQueue>"

-- | Atomically writes to the queue.
--
-- @since 0.1.0.0
writeQueue :: MonadIO m => LogTextQueue -> Log -> m ()
writeQueue queue = liftIO . (writeq <=< formatFileLog)
  where
    writeq = STM.atomically . TBQueue.writeTBQueue (queue ^. #getLogTextQueue)

-- | Atomically reads from the queue. Does not retry.
--
-- @since 0.1.0.0
readQueue :: MonadIO m => LogTextQueue -> m (Maybe LogText)
readQueue = liftIO . STM.atomically . TBQueue.tryReadTBQueue . view #getLogTextQueue

-- | Atomically flushes the queue's entire contents. Does not retry.
--
-- @since 0.1.0.0
flushQueue :: MonadIO m => LogTextQueue -> m [LogText]
flushQueue = liftIO . STM.atomically . STM.flushTBQueue . view #getLogTextQueue
