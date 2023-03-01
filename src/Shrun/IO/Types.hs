-- | Provides types for typical "IO" processes.
--
-- @since 0.8
module Shrun.IO.Types
  ( Stdout (..),
    Stderr (..),
    ReadHandleResult (..),
    readHandleResultToStderr,
    readHandle,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.HandleReader
  ( MonadHandleReader (hIsClosed),
    hGetNonBlocking,
    hIsReadable,
  )
import Shrun.Prelude

-- | Newtype wrapper for stdout.
--
-- @since 0.1
newtype Stdout = MkStdout
  { -- | @since 0.1
    getStdout :: Text
  }

-- | Newtype wrapper for stderr.
--
-- @since 0.1
newtype Stderr = MkStderr
  { -- | @since 0.1
    getStderr :: Text
  }

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
--
-- @since 0.1
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    --
    -- @since 0.1
    ReadErr Text
  | -- | Successfully read data from the handle.
    --
    -- @since 0.1
    ReadSuccess Text
  | -- | Successfully read no data from the handle.
    --
    -- @since 0.1
    ReadNoData
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Semigroup ReadHandleResult where
  ReadErr l <> _ = ReadErr l
  _ <> ReadErr r = ReadErr r
  ReadSuccess l <> _ = ReadSuccess l
  _ <> ReadSuccess r = ReadSuccess r
  _ <> _ = ReadNoData

-- | @since 0.1
instance Monoid ReadHandleResult where
  mempty = ReadNoData

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
--
-- @since 0.1
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr err

-- | Attempts to read from the handle.
--
-- @since 0.1
readHandle :: (MonadCatch m, MonadHandleReader m) => Handle -> m ReadHandleResult
readHandle handle = do
  (isClosed, canRead) <-
    (,)
      <$> hIsClosed handle
      <*> hIsReadable handle
  if
      | isClosed ->
          pure $ ReadErr "Handle closed"
      | not canRead ->
          pure $ ReadErr "Cannot read from handle"
      | otherwise -> do
          output :: Either SomeException ByteString <-
            tryAny $ hGetNonBlocking handle blockSize
          let outDecoded = fmap decodeUtf8Lenient output
          case outDecoded of
            Left ex -> pure $ ReadErr $ "Handle exception:" <> T.pack (displayException ex)
            Right "" -> pure ReadNoData
            Right o -> pure $ ReadSuccess o

blockSize :: Int
blockSize = 1024
