{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types for the legend.
module Shrun.Configuration.Toml.Legend
  ( -- * Map
    Legend (..),

    -- ** Indices
    LegendPhase (..),

    -- ** Aliases
    TomlGlobal,
    TomlLocal,
    LegendMap,
    LegendMapGlobal,
    LegendMapLocal,

    -- ** Type families
    LegendF,

    -- * Functions
    prettyLegendMap,
    difference,
    displayJsonOut,

    -- * KeyVal
    KeyVal (MkKeyVal),
    mkKeyVal,
    unsafeKeyVal,
  )
where

import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty (Config (confCompare))
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KMap
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Data.Ord (Ordering (GT, LT), compare)
import Data.Text qualified as T
import GHC.Exts (IsList (fromList))
import Prettyprinter qualified as Pretty
import Shrun.Configuration.Data.Graph (EdgeArgs)
import Shrun.Configuration.Toml (Toml)
import Shrun.Configuration.Toml.KeyVal
  ( KeyVal (MkKeyVal),
    mkKeyVal,
    unsafeKeyVal,
  )
import Shrun.Prelude

-- | Legend's scope.
data LegendScope
  = -- | Global scope refers to legends from "global configs" i.e. everything
    -- but cwd legends (e.g. xdg/config.toml, explicit -c args).
    LegendScopeGlobal
  | -- | "Local configs" i.e. ./.shrun.toml and ./shrun.toml.
    LegendScopeLocal

-- | Legend's phase.
data LegendPhase
  = -- | The entire toml file i.e. the legend is a list of key/val/edges.
    LegendPhaseToml
  | -- | The key/val/edges list after map translation.
    LegendPhaseMap

-- | Maps legend phase to its type.
type LegendF :: LegendPhase -> Type -> Type
type family LegendF p nenv where
  LegendF LegendPhaseToml nenv = Toml nenv
  LegendF LegendPhaseMap _ = HashMap Text (Tuple2 (NESeq Text) (Maybe EdgeArgs))

-- | The command legend used by the application. Has indexes for:
--
-- - Scope: Need to distinguish global/local for expanding aliases and saving
--          key cache.
--
-- - Phase: Data evolution.
type Legend :: LegendPhase -> LegendScope -> Type -> Type
newtype Legend p s nenv = MkLegend
  { unLegend :: LegendF p nenv
  }

makeFieldLabelsNoPrefix ''Legend

instance
  (Semigroup (LegendF p nenv)) =>
  Semigroup (Legend p s nenv)
  where
  MkLegend l <> MkLegend r = MkLegend (l <> r)

instance
  (Monoid (LegendF p nenv)) =>
  Monoid (Legend p s nenv)
  where
  mempty = MkLegend mempty

type TomlGlobal nenv = Legend LegendPhaseToml LegendScopeGlobal nenv

type TomlLocal nenv = Legend LegendPhaseToml LegendScopeLocal nenv

type LegendMap s nenv = Legend LegendPhaseMap s nenv

type LegendMapGlobal nenv = LegendMap LegendScopeGlobal nenv

type LegendMapLocal nenv = LegendMap LegendScopeLocal nenv

-- | Subtracts local keys from global keys.
difference :: LegendMapGlobal nenv -> LegendMapLocal nenv -> LegendMapGlobal nenv
difference (MkLegend g) (MkLegend l) = MkLegend $ g `HMap.difference` l

-- | Displays global and local keys as json.
displayJsonOut :: Maybe (LegendMapGlobal nenv) -> Maybe (LegendMapLocal nenv) -> ByteString
displayJsonOut globals locals =
  BSL.toStrict
    . AsnPretty.encodePretty' jsonCfg
    $ allJson
  where
    globalsJson = toAeson $ fromMaybe mempty globals
    localsJson = toAeson $ fromMaybe mempty locals

    allJson =
      Asn.Object
        . KMap.insert (Key.fromText "globals") globalsJson
        . KMap.insert (Key.fromText "locals") localsJson
        $ KMap.empty

    -- We want 'edges' to be last key.
    jsonCfg =
      jsonDefCfg
        { confCompare = \cases
            _ "edges" -> LT
            "edges" _ -> GT
            k1 k2 -> k1 `compare` k2
        }

toAeson :: LegendMap s nenv -> Asn.Value
toAeson =
  Asn.Array
    . fromList
    . fmap toObj
    -- Sort by keys.
    . L.sortOn (\(k, _, _) -> k)
    . HMap.foldlWithKey' go []
    . view #unLegend
  where
    toObj (key, vals, edges) =
      Asn.Object
        . KMap.insert (Key.fromText "key") (Asn.String key)
        . KMap.insert (Key.fromText "val") (Asn.String vals)
        . insEdges
        $ KMap.empty
      where
        insEdges = case edges of
          Nothing -> id
          Just es ->
            KMap.insert (Key.fromText "edges") (Asn.String es)

    go acc key val = toDisp key val : acc

    toDisp :: Text -> Tuple2 (NESeq Text) (Maybe EdgeArgs) -> Tuple3 Text Text (Maybe Text)
    toDisp key (vals, edges) =
      ( key,
        T.intercalate ", " $ toList vals,
        prettyToText <$> edges
      )

prettyLegendMap :: HashMap Text (NESeq Text, Maybe EdgeArgs) -> Doc ann
prettyLegendMap =
  vcat
    . fmap pItem
    . L.sortOn fst
    . HMap.toList
  where
    pItem :: (Text, (NESeq Text, Maybe EdgeArgs)) -> Doc ann
    pItem (k, (vals, mEdges)) =
      let pkey = Just $ "- key:   " <> pretty k
          pvals =
            Just
              $ hcat
                [ "  vals:  ",
                  pretty (T.intercalate ", " $ toList vals)
                ]
          pedges = case mEdges of
            Nothing -> Nothing
            Just es -> Just $ "  edges:" <+> pretty es
       in vcat
            $ catMaybes
              [ pkey,
                pvals,
                pedges,
                Just Pretty.softline
              ]
