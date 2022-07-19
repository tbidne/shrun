{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides types for the legend.
--
-- @since 0.1
module ShellRun.Data.Legend
  ( LegendMap,
    KeyVal,
    unsafeKeyVal,
  )
where

import Data.HashMap.Strict (HashMap)
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..), singleton)
import ShellRun.Prelude

-- | Alias for our legend map.
--
-- @since 0.1
type LegendMap = HashMap Text (NonEmptySeq Text)

-- | Holds a map key/val pair.
--
-- @since 0.5
data KeyVal = MkKeyValue
  { key :: Text,
    val :: NonEmptySeq Text
  }
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
instance
  (k ~ A_Getter, a ~ Text, b ~ Text) =>
  LabelOptic "key" k KeyVal KeyVal a b
  where
  labelOptic = to (\(MkKeyValue k _) -> k)

-- | @since 0.5
instance
  (k ~ A_Getter, a ~ NonEmptySeq Text, b ~ NonEmptySeq Text) =>
  LabelOptic "val" k KeyVal KeyVal a b
  where
  labelOptic = to (\(MkKeyValue _ v) -> v)

instance DecodeTOML KeyVal where
  tomlDecoder =
    MkKeyValue
      <$> decodeKey
      <*> decodeVal

unsafeKeyVal :: HasCallStack => Text -> NonEmptySeq Text -> KeyVal
unsafeKeyVal "" _ = error "empty key"
unsafeKeyVal k vals = case traverse testNE vals of
  Just vals' -> MkKeyValue k vals'
  Nothing -> error "empty val"

decodeKey :: Decoder Text
decodeKey = getFieldWith decodeNonEmptyText "key"

decodeVal :: Decoder (NonEmptySeq Text)
decodeVal = getFieldWith (decodeArray <|> fmap singleton decodeNonEmptyText) "val"

decodeArray :: Decoder (NonEmptySeq Text)
decodeArray =
  tomlDecoder >>= traverse testNE .> \case
    Just xs -> pure xs
    Nothing -> fail "Unexpected empty val"

decodeNonEmptyText :: Decoder Text
decodeNonEmptyText =
  tomlDecoder >>= \case
    "" -> fail "Unexpected empty text"
    other -> pure other

testNE :: Text -> Maybe Text
testNE "" = Nothing
testNE t = Just t
