{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.LegendKeysCache
  ( -- * Config action
    LegendKeysCache (..),
    parseLegendKeysCache,
    lksMeta,

    -- * Cache
    KeyCache (..),
    addKeyCache,
    mkKeyCache,
  )
where

import Data.Aeson (KeyValue ((.=)), (.:))
import Data.Aeson qualified as Asn
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Determines how to handle the legend key cache.
data LegendKeysCache
  = -- | New legend keys are added to the cache.
    LegendKeysAdd
  | -- | The cache is deleted, if it exists.
    LegendKeysClear
  | -- | Do nothing.
    LegendKeysOff
  | -- | New legend keys are written to the cache. Previous values are
    -- dropped.
    LegendKeysWrite
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML LegendKeysCache where
  tomlDecoder = parseLegendKeysCache tomlDecoder

instance Default LegendKeysCache where
  def = LegendKeysAdd

instance Pretty LegendKeysCache where
  pretty = \case
    LegendKeysAdd -> "add"
    LegendKeysClear -> "clear"
    LegendKeysOff -> "off"
    LegendKeysWrite -> "write"

parseLegendKeysCache :: (MonadFail m) => m Text -> m LegendKeysCache
parseLegendKeysCache = (>>= Utils.inversePrettyFail "legend-key-cache" lksMeta)
{-# INLINEABLE parseLegendKeysCache #-}

-- | Available 'LegendKeysCache' strings.
lksMeta :: (IsString a) => Tuple2 Bool (List a)
lksMeta = (True, ["add", "clear", "write"])

data KeyCache = MkKeyCache
  { global :: Set Text,
    local :: Map OsPath (Set Text)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''KeyCache

instance Semigroup KeyCache where
  l <> r =
    MkKeyCache
      { global = Set.union l.global r.global,
        local = Map.union l.local r.local
      }

instance Monoid KeyCache where
  mempty = MkKeyCache mempty mempty

instance FromJSON KeyCache where
  parseJSON = Asn.withObject "KeyCache" $ \v -> do
    global <- v .: "global"
    MkLocalKeyCacheMap l <- v .: "local"
    pure
      $ MkKeyCache
        { global,
          local = l
        }

instance ToJSON KeyCache where
  toJSON cache =
    Asn.object
      [ "global" .= cache.global,
        "local" .= MkLocalKeyCacheMap cache.local
      ]

newtype LocalKeyCacheMap = MkLocalKeyCacheMap
  { unLocalKeyCacheMap :: Map OsPath (Set Text)
  }

instance FromJSON LocalKeyCacheMap where
  parseJSON v = do
    vals <- fmap f <$> parseJSON v

    pure $ MkLocalKeyCacheMap $ Map.fromList vals
    where
      f :: LocalKeyCache -> Tuple2 OsPath (Set Text)
      f lkc = (lkc.path, lkc.keys)

instance ToJSON LocalKeyCacheMap where
  toJSON lkcm = toJSON $ f <$> Map.toList lkcm.unLocalKeyCacheMap
    where
      f (p, ks) = MkLocalKeyCache p ks

data LocalKeyCache = MkLocalKeyCache
  { path :: OsPath,
    keys :: Set Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON LocalKeyCache where
  parseJSON = Asn.withObject "LocalKeyCache" $ \v -> do
    path <- encodeFail =<< v .: "path"
    keys <- v .: "keys"

    pure
      $ MkLocalKeyCache
        { path,
          keys
        }

instance ToJSON LocalKeyCache where
  toJSON lkc =
    Asn.object
      [ "path" .= unsafeDecode lkc.path,
        "keys" .= lkc.keys
      ]

addKeyCache :: Set Text -> Tuple2 OsPath (Set Text) -> KeyCache -> KeyCache
addKeyCache globalKeys (path, localKeys) kc =
  MkKeyCache
    { global = Set.union kc.global globalKeys,
      local =
        -- If local keys are empty, do nothing.
        if Set.null localKeys
          then kc.local
          else Map.insertWith Set.union path localKeys kc.local
    }

mkKeyCache :: Set Text -> Tuple2 OsPath (Set Text) -> KeyCache
mkKeyCache globalKeys (path, localKeys) =
  MkKeyCache
    { global = globalKeys,
      local =
        -- Do not add keys unless non-empty
        if Set.null localKeys
          then Map.empty
          else Map.singleton path localKeys
    }
