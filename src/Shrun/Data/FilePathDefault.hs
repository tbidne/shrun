-- | Provides the 'FilePathDefault' type.
module Shrun.Data.FilePathDefault
  ( FilePathDefault (..),
  )
where

import Data.Char qualified as Ch
import Effects.FileSystem.Utils qualified as FsUtils
import Shrun.Prelude

-- | OsPath option that includes a default possibility.
data FilePathDefault
  = FPDefault
  | FPManual !OsPath
  deriving stock (Eq, Show)

instance DecodeTOML FilePathDefault where
  tomlDecoder = do
    f <- tomlDecoder
    case fmap Ch.toLower f of
      "default" -> pure FPDefault
      "" -> fail "Empty path given for --file-log"
      _ -> FPManual <$> FsUtils.encodeFpToOsFail f
