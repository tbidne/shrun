-- | Provides the 'FilePathDefault' type.
module Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault (..),
    parseFilePathDefault,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Shrun.Prelude

-- | OsPath option that includes a default possibility.
data FilePathDefault
  = FPDefault
  | FPManual OsPath
  deriving stock (Eq, Show)

instance DecodeTOML FilePathDefault where
  tomlDecoder = parseFilePathDefault tomlDecoder

parseFilePathDefault :: (MonadFail m) => m Text -> m FilePathDefault
parseFilePathDefault getTxt =
  getTxt >>= \case
    "default" -> pure FPDefault
    "" -> fail "Empty path given for --file-log"
    other -> FPManual <$> FsUtils.encodeFpToOsFail (T.unpack other)
{-# INLINEABLE parseFilePathDefault #-}
