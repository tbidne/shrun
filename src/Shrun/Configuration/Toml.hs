{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Toml
  ( Toml (..),
    mergeTomls,
  )
where

import Data.HashSet qualified as HSet
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
    CoreConfigToml,
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Configuration.Toml.Legend (KeyVal)
import Shrun.Prelude

-- | Holds toml config.
data Toml = MkToml
  { -- | Core config.
    coreConfig :: CoreConfigToml,
    -- | Legend.
    legend :: Maybe (Seq KeyVal)
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ CoreConfigToml, b ~ CoreConfigToml) =>
  LabelOptic "coreConfig" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml a1 a2) ->
    fmap (\b -> MkToml b a2) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe (Seq KeyVal), b ~ Maybe (Seq KeyVal)) =>
  LabelOptic "legend" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml a1 a2) ->
    fmap (\b -> MkToml a1 b) (f a2)
  {-# INLINE labelOptic #-}

instance DecodeTOML Toml where
  tomlDecoder = do
    timeout <- decodeTimeout
    init <- decodeInit
    commonLogging <- getFieldOptWith tomlDecoder "common-log"
    commandLogging <- getFieldOptWith tomlDecoder "command-log"
    consoleLogging <- getFieldOptWith tomlDecoder "console-log"
    fileLogging <- getFieldOptWith tomlDecoder "file-log"
    notify <- getFieldOptWith tomlDecoder "notify"

    legend <- decodeLegend
    pure
      $ MkToml
        { coreConfig =
            MkCoreConfigP
              { timeout,
                init,
                commonLogging,
                commandLogging,
                consoleLogging,
                fileLogging,
                notify
              },
          legend
        }

decodeTimeout :: Decoder (Maybe (WithDisabled Timeout))
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

decodeInit :: Decoder (Maybe (WithDisabled Text))
decodeInit = getFieldOptWith tomlDecoder "init"

decodeLegend :: Decoder (Maybe (Seq KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"

-- | Note that our Semigroup is /not/ commutative, hence the order matters.
-- In particular, mconcat is safe because it is foldr, hence respects the
-- input order.
mergeTomls :: Seq Toml -> Toml
mergeTomls = mconcat . toList

-- NOTE: [Toml Semigroup]
--
-- The Toml semigroup is used to combine multiple tomls into a single toml,
-- for further combining with CLI args. The general strategy is to take
-- the "more defined" fields i.e. if an element is set on the LHS or RHS,
-- we take it. The instance is left-biased in the sense that we favor the
-- LHS when there is a conflict i.e. both elements have defined the same
-- field.
--
-- This is essentially Maybe's Alternative instance semantics. In fact,
-- since every field is ultimately a Maybe (all fields are optional),
-- most fields are indeed using Maybe's (<|>) and empty.
--
-- It is only "non-scaler" types (i.e. aggregate types like FileLogging) that
-- declare their own Semigroup/Monoid instances, in terms of their fields'
-- Alternative instances. This way we ensure we "reach down" deeply into
-- each field, when they exist.
--
-- The LHS bias is actually designed to favor the /right-most/ element
-- in the CLI, with the potential xdg at the very left e.g.
--
--   xdg, cfg1, cfg2, ..., cfgn
--
-- See NOTE: [Toml order] for more details.
--
-- Also, because we want Tomls to use Alternative or Monoid instances, we
-- deliberately do not provide Default instances for non-scalar types e.g.
-- CommandLogging. Instances should exist only for *Args, as Merged does
-- not need them either.

instance Semigroup Toml where
  l <> r =
    MkToml
      { coreConfig = l ^. #coreConfig <> r ^. #coreConfig,
        legend = unionLegend (l ^. #legend) (r ^. #legend)
      }
    where
      unionLegend Nothing Nothing = Nothing
      unionLegend (Just xs) Nothing = Just xs
      unionLegend Nothing (Just ys) = Just ys
      unionLegend (Just xs) (Just ys) = Just (uniqKeys xs ys)

      -- When combining two files' legends, we want the following behavor:
      --
      --   1. LHS keys override the RHS.
      --   2. Duplicates /within the same legend/ are allowed (assuming
      --     RHS duplicates are not overwritten).
      --
      -- We want 2 so that we later report an error for any found duplicates
      -- in the same config, as this is likely an error. But duplicates across
      -- multiple configs should merely override.
      --
      -- Hence we take everything from the LHS, then add everything from the
      -- RHS that is not a duplicate.
      uniqKeys xs ys =
        let lhsKeys = HSet.fromList (toList $ view #key <$> xs)
         in foldl' (go lhsKeys) xs ys

      go lhsKeys acc kv
        | HSet.member key lhsKeys = acc
        | otherwise = acc :|> kv
        where
          key :: Text
          key = kv ^. #key

instance Monoid Toml where
  mempty =
    MkToml
      { coreConfig = mempty,
        legend = Nothing
      }
