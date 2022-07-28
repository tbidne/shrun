-- | Provides the 'FileSystemReader' typeclass.
--
-- @since 0.5
module Shrun.Effects.FileSystemReader
  ( FileSystemReader (..),
    getShrunXdgConfig,
  )
where

import Data.Bytes (Bytes (..), Size (B))
import Options.Applicative (execParser)
import Shrun.Configuration.Args (Args, parserInfoArgs)
import Shrun.Prelude
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.5
class Monad m => FileSystemReader m where
  -- | Returns the default directory e.g. Xdg config dir.
  --
  -- @since 0.5
  getXdgConfig :: FilePath -> m FilePath

  -- | Reads a file.
  --
  -- @since 0.5
  readFile :: FilePath -> m Text

  -- | Gets the file size.
  --
  -- @since 0.5
  getFileSize :: FilePath -> m (Bytes B Natural)

  -- | Test for file existence.
  --
  -- @since 0.5
  doesFileExist :: FilePath -> m Bool

  -- | Get CLI args.
  --
  -- @since 0.5
  getArgs :: m Args

-- | @since 0.5
instance FileSystemReader IO where
  getXdgConfig = Dir.getXdgDirectory XdgConfig
  readFile = readFileUtf8Lenient
  doesFileExist = Dir.doesFileExist
  getArgs = execParser parserInfoArgs

  getFileSize fp = do
    bytes <- Dir.getFileSize fp
    if bytes < 0
      then
        throwString $
          mconcat
            [ "File '",
              fp,
              "' has negative size: ",
              show bytes
            ]
      else pure $ MkBytes (fromIntegral bytes)

-- | @since 0.5
instance FileSystemReader m => FileSystemReader (ReaderT env m) where
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
  getFileSize = lift . getFileSize
  doesFileExist = lift . doesFileExist
  getArgs = lift getArgs

-- | Gets the Xdg config directory for the shrun application.
--
-- @since 0.5
getShrunXdgConfig :: FileSystemReader m => m FilePath
getShrunXdgConfig = getXdgConfig "shrun"
