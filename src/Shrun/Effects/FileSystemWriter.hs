-- | Provides the 'FileSystemWriter' typeclass.
--
-- @since 0.3
module Shrun.Effects.FileSystemWriter
  ( FileSystemWriter (..),
  )
where

import Shrun.Prelude

-- | Represents a writable filesystem.
--
-- @since 0.5
class Monad m => FileSystemWriter m where
  -- | Appends the text to the file.
  --
  -- @since 0.5
  appendFile :: FilePath -> Text -> m ()

-- | @since 0.5
instance FileSystemWriter IO where
  appendFile = appendFileUtf8

-- | @since 0.5
instance FileSystemWriter m => FileSystemWriter (ReaderT env m) where
  appendFile fp = lift . appendFile fp
