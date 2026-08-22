{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging
  ( -- * Types
    Debug (..),
    CommonLoggingP (..),
    CommonLoggingArgs,
    CommonLoggingToml,
    CommonLoggingMerged,
    CommonLoggingEnv,

    -- * Functions
    mergeCommonLogging,
    toEnv,
  )
where

import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch (KeyHideSwitch)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    SwitchF,
    parseSwitch,
  )
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Prelude

newtype Debug = MkDebug {unDebug :: Bool}
  deriving stock (Eq, Show)
  deriving (Pretty) via PrettySwitch

makeFieldLabelsNoPrefix ''Debug

instance Default Debug where
  def = MkDebug False

instance DecodeTOML Debug where
  tomlDecoder = MkDebug <$> (tomlDecoder >>= parseSwitch)

-- | Holds command logging config.
type CommonLoggingP :: ConfigPhase -> Type
data CommonLoggingP p = MkCommonLoggingP
  { -- | Whether debug logs are on.
    debug :: SwitchF p Debug,
    -- | Whether to display command by (key) name or command.
    keyHide :: SwitchF p KeyHideSwitch
  }

makeFieldLabelsNoPrefix ''CommonLoggingP

instance Semigroup CommonLoggingToml where
  l <> r =
    MkCommonLoggingP
      { debug = l ^. #debug <|> r ^. #debug,
        keyHide = l ^. #keyHide <|> r ^. #keyHide
      }

instance Monoid CommonLoggingToml where
  mempty =
    MkCommonLoggingP
      { debug = Nothing,
        keyHide = Nothing
      }

instance Pretty CommonLoggingMerged where
  pretty c =
    vcat
      [ "debug: " <> pretty (c ^. #debug),
        "key-hide: " <> pretty (c ^. #keyHide)
      ]

type CommonLoggingArgs = CommonLoggingP ConfigPhaseArgs

type CommonLoggingToml = CommonLoggingP ConfigPhaseToml

type CommonLoggingMerged = CommonLoggingP ConfigPhaseMerged

type CommonLoggingEnv = CommonLoggingP ConfigPhaseEnv

deriving stock instance Eq (CommonLoggingP ConfigPhaseArgs)

deriving stock instance Show (CommonLoggingP ConfigPhaseArgs)

deriving stock instance Eq (CommonLoggingP ConfigPhaseToml)

deriving stock instance Show (CommonLoggingP ConfigPhaseToml)

deriving stock instance Eq (CommonLoggingP ConfigPhaseMerged)

deriving stock instance Show (CommonLoggingP ConfigPhaseMerged)

instance Default CommonLoggingArgs where
  def = MkCommonLoggingP Nothing Nothing

-- | Merges args and toml configs.
mergeCommonLogging ::
  CommonLoggingArgs ->
  Maybe CommonLoggingToml ->
  CommonLoggingMerged
mergeCommonLogging args mToml =
  MkCommonLoggingP
    { debug =
        (args ^. #debug) <.> (toml ^. #debug),
      keyHide =
        (args ^. #keyHide) <.> (toml ^. #keyHide)
    }
  where
    toml = fromMaybe mempty mToml

instance DecodeTOML CommonLoggingToml where
  tomlDecoder =
    MkCommonLoggingP
      <$> getFieldOptWith tomlDecoder "debug"
      <*> decodeKeyHideSwitch

decodeKeyHideSwitch :: Decoder (Maybe KeyHideSwitch)
decodeKeyHideSwitch = getFieldOptWith tomlDecoder "key-hide"

-- | Creates env version from merged.
toEnv :: CommonLoggingMerged -> CommonLoggingEnv
toEnv merged =
  MkCommonLoggingP
    { debug = merged ^. #debug,
      keyHide = merged ^. #keyHide
    }
