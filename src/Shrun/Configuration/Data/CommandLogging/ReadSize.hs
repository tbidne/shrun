{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadSize
  ( ReadSize (..),
    parseReadSize,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Read size for command logs.
newtype ReadSize = MkReadSize {unReadSize :: Bytes B Natural}
  deriving stock (Eq, Show)

instance
  ( k ~ An_Iso,
    a ~ Bytes B Natural,
    b ~ Bytes B Natural
  ) =>
  LabelOptic
    "unReadSize"
    k
    ReadSize
    ReadSize
    a
    b
  where
  labelOptic = iso (\(MkReadSize x) -> x) MkReadSize

instance Default ReadSize where
  def = MkReadSize $ MkBytes 1_000

instance DecodeTOML ReadSize where
  tomlDecoder = parseReadSize tomlDecoder

parseReadSize :: (MonadFail m) => m Text -> m ReadSize
parseReadSize getTxt = do
  byteTxt <- getTxt
  case U.parseByteText byteTxt of
    Right b -> pure $ MkReadSize b
    Left err -> fail $ "Could not parse --command-log-read-size size: " <> unpack err
