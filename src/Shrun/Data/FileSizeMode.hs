module Shrun.Data.FileSizeMode
  ( FileSizeMode (..),
    parseFileSizeMode,
    defaultFileSizeMode,
    expectedStr,
  )
where

import Data.Bytes (Conversion (convert))
import Data.Bytes.Size (Size (M))
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
  | -- | Does nothing.
    FileSizeModeNothing
  deriving stock (Eq, Show)

instance DecodeTOML FileSizeMode where
  tomlDecoder = parseFileSizeMode tomlDecoder

parseFileSizeMode :: (MonadFail m) => m Text -> m FileSizeMode
parseFileSizeMode getTxt = do
  txt <- getTxt
  if txt == "nothing"
    then pure FileSizeModeNothing
    else do
      let (m, byteTxt) = T.break Ch.isSpace txt
      cons <- case m of
        "warn" -> pure FileSizeModeWarn
        "delete" -> pure FileSizeModeDelete
        bad ->
          fail
            $ mconcat
              [ "Expected file-log-size-mode as one of ",
                expectedStr,
                " received: '",
                unpack bad,
                "'"
              ]
      case U.parseByteText byteTxt of
        Right b -> pure $ cons b
        Left err -> fail $ "Could not parse --file-log-size-mode size: " <> unpack err

-- | Warns at 50 mb.
defaultFileSizeMode :: FileSizeMode
defaultFileSizeMode = FileSizeModeWarn $ convert (Proxy @B) defBytes
  where
    defBytes :: Bytes M Natural
    defBytes = MkBytes 50

expectedStr :: String
expectedStr = "(warn SIZE | delete SIZE | nothing)"
