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
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.WithDisabled ((<.>?))
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

newtype Debug = MkDebug {unDebug :: Bool}
  deriving stock (Eq, Show)

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic "unDebug" k Debug Debug a b
  where
  labelOptic = iso (\(MkDebug b) -> b) MkDebug
  {-# INLINE labelOptic #-}

instance Default Debug where
  def = MkDebug False

instance DecodeTOML Debug where
  tomlDecoder = MkDebug <$> tomlDecoder

-- | Holds command logging config.
type CommonLoggingP :: ConfigPhase -> Type
data CommonLoggingP p = MkCommonLoggingP
  { -- | Whether debug logs are on.
    debug :: ConfigPhaseF p Debug,
    -- | Whether to display command by (key) name or command.
    keyHide :: ConfigPhaseF p KeyHideSwitch
  }

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p Debug, b ~ ConfigPhaseF p Debug) =>
  LabelOptic "debug" k (CommonLoggingP p) (CommonLoggingP p) a b
  where
  labelOptic =
    lensVL $ \f (MkCommonLoggingP a1 a2) ->
      fmap
        (\b -> MkCommonLoggingP b a2)
        (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p KeyHideSwitch, b ~ ConfigPhaseF p KeyHideSwitch) =>
  LabelOptic "keyHide" k (CommonLoggingP p) (CommonLoggingP p) a b
  where
  labelOptic =
    lensVL $ \f (MkCommonLoggingP a1 a2) ->
      fmap
        (\b -> MkCommonLoggingP a1 b)
        (f a2)
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

instance
  (Default (ConfigPhaseF p Debug), Default (ConfigPhaseF p KeyHideSwitch)) =>
  Default (CommonLoggingP p)
  where
  def = MkCommonLoggingP def def

-- | Merges args and toml configs.
mergeCommonLogging ::
  CommonLoggingArgs ->
  Maybe CommonLoggingToml ->
  CommonLoggingMerged
mergeCommonLogging args mToml =
  MkCommonLoggingP
    { debug =
        (args ^. #debug) <.>? (toml ^. #debug),
      keyHide =
        (args ^. #keyHide) <.>? (toml ^. #keyHide)
    }
  where
    toml = fromMaybe def mToml

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
