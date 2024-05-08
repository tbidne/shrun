{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging
  ( -- * Types
    CommonLoggingP (..),
    CommonLoggingArgs,
    CommonLoggingToml,
    CommonLoggingMerged,
    CommonLoggingEnv,

    -- * Functions
    mergeCommonLogging,
    toEnv,

    -- * Misc
    defaultMerged,
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
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.WithDisabled ((<>?.))
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Holds command logging config.
type CommonLoggingP :: ConfigPhase -> Type
newtype CommonLoggingP p = MkCommonLoggingP
  { -- | Whether to display command by (key) name or command.
    keyHide :: ConfigPhaseF p KeyHideSwitch
  }

instance
  (k ~ An_Iso, a ~ ConfigPhaseF p KeyHideSwitch, b ~ ConfigPhaseF p KeyHideSwitch) =>
  LabelOptic "keyHide" k (CommonLoggingP p) (CommonLoggingP p) a b
  where
  labelOptic = iso (\(MkCommonLoggingP kh) -> kh) MkCommonLoggingP
  {-# INLINE labelOptic #-}

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

-- | Merges args and toml configs.
mergeCommonLogging ::
  CommonLoggingArgs ->
  Maybe CommonLoggingToml ->
  CommonLoggingMerged
mergeCommonLogging args mToml =
  MkCommonLoggingP
    { keyHide =
        (args ^. #keyHide) <>?. (toml ^. #keyHide)
    }
  where
    toml = fromMaybe defaultToml mToml

instance DecodeTOML CommonLoggingToml where
  tomlDecoder =
    MkCommonLoggingP
      <$> decodeKeyHideSwitch

decodeKeyHideSwitch :: Decoder (Maybe KeyHideSwitch)
decodeKeyHideSwitch = getFieldOptWith tomlDecoder "key-hide"

-- | Creates env version from merged.
toEnv :: CommonLoggingMerged -> CommonLoggingEnv
toEnv merged =
  MkCommonLoggingP
    { keyHide = merged ^. #keyHide
    }

defaultToml :: CommonLoggingToml
defaultToml =
  MkCommonLoggingP
    { keyHide = Nothing
    }

defaultMerged :: CommonLoggingMerged
defaultMerged =
  MkCommonLoggingP
    { keyHide = def
    }
