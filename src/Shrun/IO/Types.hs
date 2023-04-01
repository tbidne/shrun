-- | Provides types for typical "IO" processes.
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
newtype Stdout = MkStdout
  { getStdout :: Text
  }

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr
  { getStderr :: Text
  }

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadErr' _ < 'ReadNoData' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    ReadErr Text
  | -- | Successfully read data from the handle.
    ReadSuccess Text
  | -- | Successfully read no data from the handle.
    ReadNoData
  deriving stock (Eq, Show)

instance Semigroup ReadHandleResult where
  ReadErr l <> _ = ReadErr l
  _ <> ReadErr r = ReadErr r
  ReadSuccess l <> _ = ReadSuccess l
  _ <> ReadSuccess r = ReadSuccess r
  _ <> _ = ReadNoData

instance Monoid ReadHandleResult where
  mempty = ReadNoData

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr "<No data>"
readHandleResultToStderr (ReadErr err) = MkStderr err
readHandleResultToStderr (ReadSuccess err) = MkStderr err

-- | Attempts to read from the handle.
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
