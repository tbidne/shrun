{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types for the legend.
--
-- @since 0.1
module Shrun.Data.Legend
  ( LegendMap,
    KeyVal (MkKeyVal),
    mkKeyVal,
    unsafeKeyVal,
  )
where

import Data.HashMap.Strict (HashMap)
import Optics.TH
  ( generateUpdateableOptics,
    makeFieldLabelsWith,
    noPrefixFieldLabels,
  )
import Shrun.Data.NonEmptySeq (NonEmptySeq (..))
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Prelude

-- | Alias for our legend map.
--
-- @since 0.1
type LegendMap = HashMap Text (NonEmptySeq Text)

-- | Holds a map key/val pair. The maintained invariants are:
--
-- * @key@ is non-empty.
-- * @val@ is non-empty.
-- * all @v_i@ in @val@ are non-empty.
--
-- @since 0.5
data KeyVal = UnsafeKeyVal
  { -- | @since 0.5
    key :: !Text,
    -- | @since 0.5
    val :: !(NonEmptySeq Text)
  }
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | Unidirectional pattern synonym for 'KeyVal'.
--
-- @since 0.5
pattern MkKeyVal :: Text -> NonEmptySeq Text -> KeyVal
pattern MkKeyVal k v <- UnsafeKeyVal k v

{-# COMPLETE MkKeyVal #-}

-- | @since 0.6.1
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''KeyVal

instance DecodeTOML KeyVal where
  tomlDecoder =
    UnsafeKeyVal
      <$> decodeKey
      <*> decodeVal

-- | Smart constructor for 'KeyVal'. Given @UnsafeKeyVal key vals@, all
-- conditions must be satisfied for success:
--
-- * @key@ is non-empty.
-- * @vals@ is non-empty.
-- * all @v_i@ in @vals@ are non-empty.
--
--
-- @since 0.5
mkKeyVal :: Text -> List Text -> Maybe KeyVal
mkKeyVal "" _ = Nothing
mkKeyVal _ [] = Nothing
mkKeyVal k vals = UnsafeKeyVal k <$> NESeq.fromList vals

-- | Variant of 'UnsafeKeyVal' that throws an error on failures.
--
-- @since 0.5
unsafeKeyVal :: HasCallStack => Text -> List Text -> KeyVal
unsafeKeyVal "" _ = error "[Shrun.Data.Legend.unsafeKeyVal]: empty key"
unsafeKeyVal k vals = case mkKeyVal k vals of
  Just kv -> kv
  Nothing -> error "[Shrun.Data.Legend.unsafeKeyVal]: empty val"

decodeKey :: Decoder Text
decodeKey = getFieldWith decodeNonEmptyText "key"

decodeVal :: Decoder (NonEmptySeq Text)
decodeVal = getFieldWith (decodeArray <|> fmap NESeq.singleton decodeNonEmptyText) "val"

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
