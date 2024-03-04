module Shrun.Data.FileSizeMode
  ( FileSizeMode (..),
    parseFileSizeMode,
  )
where

import Data.Char qualified as Ch
import Data.Text qualified as T
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Determines what to do if the log file surpasses the given size
-- threshold.
data FileSizeMode
  = -- | Print a warning.
    FileSizeModeWarn (Bytes B Natural)
  | -- | Delete the file.
    FileSizeModeDelete (Bytes B Natural)
  deriving stock (Eq, Show)

instance DecodeTOML FileSizeMode where
  tomlDecoder = parseFileSizeMode tomlDecoder

parseFileSizeMode :: (MonadFail m) => m Text -> m FileSizeMode
parseFileSizeMode getTxt = do
  txt <- getTxt
  let (m, byteTxt) = T.break Ch.isSpace txt
  cons <- case m of
    "warn" -> pure FileSizeModeWarn
    "delete" -> pure FileSizeModeDelete
    bad -> fail $ "Unrecognized file-log-size-mode: " <> unpack bad
  case U.parseByteText byteTxt of
    Right b -> pure $ cons b
    Left err -> fail $ "Could not parse --file-log-size-mode size: " <> unpack err
