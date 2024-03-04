module Shrun.Data.FileMode
  ( FileMode (..),
    parseFileMode,
  )
where

import Shrun.Prelude

-- | File mode.
data FileMode
  = FileModeAppend
  | FileModeWrite
  deriving stock (Eq, Show)

instance DecodeTOML FileMode where
  tomlDecoder = parseFileMode tomlDecoder

parseFileMode :: (MonadFail m) => m Text -> m FileMode
parseFileMode getTxt =
  getTxt >>= \case
    "append" -> pure FileModeAppend
    "write" -> pure FileModeWrite
    bad -> fail $ "Unrecognized file-mode: " <> unpack bad
