module Shrun.Configuration.Data.LegendKeysCache
  ( LegendKeysCache (..),
    parseLegendKeysCache,
    lksStrings,
  )
where

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

parseLegendKeysCache :: (MonadFail m) => m Text -> m LegendKeysCache
parseLegendKeysCache getTxt =
  getTxt >>= \case
    "add" -> pure LegendKeysAdd
    "clear" -> pure LegendKeysClear
    "write" -> pure LegendKeysWrite
    "off" -> pure LegendKeysOff
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "legend-key-cache"
          lksStrings
          (unpack bad)
{-# INLINEABLE parseLegendKeysCache #-}

-- | Available 'LegendKeysCache' strings.
lksStrings :: (IsString a) => a
lksStrings = "(add | clear | write | off)"
