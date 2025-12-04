-- | Provides the 'FilePathDefault' type.
module Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault (..),
    parseFilePathDefault,
    _FPDefault,
    _FPManual,
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

instance Pretty FilePathDefault where
  pretty = \case
    FPDefault -> "default"
    FPManual p -> pretty $ decodeLenient p

parseFilePathDefault :: (MonadFail m) => Text -> m FilePathDefault
parseFilePathDefault = \case
  "default" -> pure FPDefault
  "" -> fail "Empty path given for --file-log"
  other -> FPManual <$> OsPath.encodeFail (T.unpack other)
{-# INLINEABLE parseFilePathDefault #-}

_FPDefault :: Prism' FilePathDefault ()
_FPDefault =
  prism
    (const FPDefault)
    ( \case
        FPDefault -> Right ()
        FPManual p -> Left (FPManual p)
    )
{-# INLINE _FPDefault #-}

_FPManual :: Prism' FilePathDefault OsPath
_FPManual =
  prism
    FPManual
    ( \case
        FPManual p -> Right p
        FPDefault -> Left FPDefault
    )
{-# INLINE _FPManual #-}
