-- | Provides the 'FileSystemReader' typeclass.
--
-- @since 0.5
module Shrun.Effects.FileSystemReader
  ( FileSystemReader (..),
    getShrunXdgConfig,
  )
where

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

-- | @since 0.5
instance FileSystemReader m => FileSystemReader (ReaderT env m) where
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  getArgs = lift getArgs

-- | Gets the Xdg config directory for the shrun application.
--
-- @since 0.5
getShrunXdgConfig :: FileSystemReader m => m FilePath
getShrunXdgConfig = getXdgConfig "shrun"
