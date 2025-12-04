module Shrun.Configuration.Data.FileLogging.FileMode
  ( FileMode (..),
    parseFileMode,
    fileModeStr,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | File mode.
data FileMode
  = FileModeAppend
  | FileModeRename
  | FileModeWrite
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML FileMode where
  tomlDecoder = parseFileMode tomlDecoder

instance Pretty FileMode where
  pretty = \case
    FileModeAppend -> "append"
    FileModeRename -> "rename"
    FileModeWrite -> "write"

parseFileMode :: (MonadFail m) => m Text -> m FileMode
parseFileMode getTxt =
  getTxt >>= \case
    "append" -> pure FileModeAppend
    "rename" -> pure FileModeRename
    "write" -> pure FileModeWrite
    bad ->
      fail
        $ Utils.fmtUnrecognizedError "file-mode" fileModeStr (unpack bad)
{-# INLINEABLE parseFileMode #-}

instance Default FileMode where
  def = FileModeWrite

fileModeStr :: (IsString a) => a
fileModeStr = "(append | rename | write)"
