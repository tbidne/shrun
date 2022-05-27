-- | Provides the 'MonadFSReader' typeclass.
--
-- @since 0.3
module ShellRun.Effects.MonadFSReader
  ( MonadFSReader (..),
  )
where

import ShellRun.Prelude
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.3
class Monad m => MonadFSReader m where
  -- | Returns the default directory e.g. Xdg config dir.
  --
  -- @since 0.3
  getXdgConfig :: FilePath -> m FilePath

  -- | Reads a file.
  --
  -- @since 0.3
  readFile :: FilePath -> m Text

-- | @since 0.3
instance MonadFSReader IO where
  getXdgConfig = Dir.getXdgDirectory XdgConfig
  readFile = readFileUtf8Lenient

-- | @since 0.3
instance MonadFSReader m => MonadFSReader (ReaderT env m) where
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
