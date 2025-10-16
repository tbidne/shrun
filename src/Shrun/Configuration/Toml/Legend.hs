{-# LANGUAGE UndecidableInstances #-}

-- | Provides types for the legend.
module Shrun.Configuration.Toml.Legend
  ( LegendMap,
    KeyVal (MkKeyVal),
    mkKeyVal,
    unsafeKeyVal,
  )
where

import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Shrun.Configuration.Args.Parsing.Graph (parseEdges)
import Shrun.Configuration.Data.Graph (EdgeArgs)
import Shrun.Prelude

-- | Alias for our legend map.
type LegendMap = HashMap Text (Tuple2 (NESeq Text) (Maybe EdgeArgs))

-- | Holds a map key/val pair. The maintained invariants are:
--
-- * @key@ is non-empty.
-- * @val@ is non-empty.
-- * all @v_i@ in @val@ are non-empty.
data KeyVal = UnsafeKeyVal
  { edges :: Maybe EdgeArgs,
    key :: Text,
    val :: NESeq Text
  }
  deriving stock (Eq, Show)

-- | Unidirectional pattern synonym for 'KeyVal'.
pattern MkKeyVal :: Maybe EdgeArgs -> Text -> NESeq Text -> KeyVal
pattern MkKeyVal es k v <- UnsafeKeyVal es k v

{-# COMPLETE MkKeyVal #-}

instance
  ( k ~ A_Getter,
    a ~ Maybe EdgeArgs,
    b ~ Maybe EdgeArgs
  ) =>
  LabelOptic "edges" k KeyVal KeyVal a b
  where
  labelOptic = to (\(UnsafeKeyVal es _ _) -> es)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Getter,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "key" k KeyVal KeyVal a b
  where
  labelOptic = to (\(UnsafeKeyVal _ k _) -> k)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Getter,
    a ~ NESeq Text,
    b ~ NESeq Text
  ) =>
  LabelOptic "val" k KeyVal KeyVal a b
  where
  labelOptic = to (\(UnsafeKeyVal _ _ v) -> v)
  {-# INLINE labelOptic #-}

instance DecodeTOML KeyVal where
  tomlDecoder =
    UnsafeKeyVal
      <$> decodeEdges
      <*> decodeKey
      <*> decodeVal

-- | Smart constructor for 'KeyVal'. Given @UnsafeKeyVal key vals@, all
-- conditions must be satisfied for success:
--
-- * @key@ is non-empty.
-- * @vals@ is non-empty.
-- * all @v_i@ in @vals@ are non-empty.
mkKeyVal :: Maybe EdgeArgs -> Text -> List Text -> Maybe KeyVal
mkKeyVal _ "" _ = Nothing
mkKeyVal _ _ [] = Nothing
mkKeyVal es k vals = UnsafeKeyVal es k <$> NESeq.nonEmptySeq (Seq.fromList vals)

{- HLINT ignore unsafeKeyVal "Redundant bracket" -}

-- | Variant of 'UnsafeKeyVal' that throws an error on failures.
unsafeKeyVal :: (HasCallStack) => Maybe EdgeArgs -> Text -> List Text -> KeyVal
unsafeKeyVal _ "" _ = error "[Shrun.Configuration.Toml.Legend.unsafeKeyVal]: empty key"
unsafeKeyVal es k vals = case mkKeyVal es k vals of
  Just kv -> kv
  Nothing -> error "[Shrun.Configuration.Toml.Legend.unsafeKeyVal]: empty val"

decodeEdges :: Decoder (Maybe EdgeArgs)
decodeEdges = getFieldOptWith d "edges"
  where
    d = do
      txt <- tomlDecoder
      case parseEdges txt of
        Left err -> fail err
        Right x -> pure x

decodeKey :: Decoder Text
decodeKey = getFieldWith decodeNonEmptyText "key"

decodeVal :: Decoder (NESeq Text)
decodeVal = getFieldWith (decodeArray <|> fmap NESeq.singleton decodeNonEmptyText) "val"

decodeArray :: Decoder (NESeq Text)
decodeArray =
  tomlDecoder
    >>= ( traverse testNE >>> \case
            Just xs -> pure $ NESeq.fromList xs
            Nothing -> fail "Unexpected empty val"
        )

decodeNonEmptyText :: Decoder Text
decodeNonEmptyText =
  tomlDecoder >>= \case
    "" -> fail "Unexpected empty text"
    other -> pure other

testNE :: Text -> Maybe Text
testNE "" = Nothing
testNE t = Just t
