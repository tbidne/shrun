-- | Provides the 'FilePathDefault' type.
module Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault (..),
    parseFilePathDefault,
  )
where

import Data.Text qualified as T
import FileSystem.OsPath qualified as OsPath
import Shrun.Prelude

-- | OsPath option that includes a default possibility.
data FilePathDefault
  = FPDefault
  | FPManual OsPath
  deriving stock (Eq, Show)

instance DecodeTOML FilePathDefault where
  tomlDecoder = tomlDecoder >>= parseFilePathDefault

parseFilePathDefault :: (MonadFail m) => Text -> m FilePathDefault
parseFilePathDefault = \case
  "default" -> pure FPDefault
  "" -> fail "Empty path given for --file-log"
  other -> FPManual <$> OsPath.encodeFail (T.unpack other)
{-# INLINEABLE parseFilePathDefault #-}
