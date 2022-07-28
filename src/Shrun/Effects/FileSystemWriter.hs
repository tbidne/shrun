-- | Provides the 'FileSystemWriter' typeclass.
--
-- @since 0.3
module Shrun.Effects.FileSystemWriter
  ( FileSystemWriter (..),
  )
where

import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TEnc
import Shrun.Prelude
import System.Directory qualified as Dir
import System.IO qualified as IO

-- | Represents a writable filesystem.
--
-- @since 0.5
class Monad m => FileSystemWriter m where
  -- | Appends the text to the file.
  --
  -- @since 0.5
  appendFile :: FilePath -> Text -> m ()

  -- | Deletes a file.
  --
  -- @since 0.5
  deleteFile :: FilePath -> m ()

  -- | Writes the text to the specified handle.
  --
  -- @since 0.5
  hPut :: Handle -> Text -> m ()

  -- | Flushes the buffer.
  --
  -- @since 0.5
  hFlush :: Handle -> m ()

  -- | Runs an action with a file handle.
  --
  -- @since 0.5
  withFile :: FilePath -> IOMode -> (Handle -> m a) -> m a

-- | @since 0.5
instance FileSystemWriter IO where
  appendFile = appendFileUtf8
  deleteFile = Dir.removeFile
  hPut h = BS.hPut h . TEnc.encodeUtf8
  hFlush = IO.hFlush
  withFile = IO.withFile

-- | @since 0.5
instance (FileSystemWriter m, MonadUnliftIO m) => FileSystemWriter (ReaderT env m) where
  appendFile fp = lift . appendFile fp
  deleteFile = lift . deleteFile
  hPut h = lift . hPut h
  hFlush = lift . hFlush
  withFile fp mode m =
    withRunInIO $ \runner ->
      withFile fp mode (runner . m)
