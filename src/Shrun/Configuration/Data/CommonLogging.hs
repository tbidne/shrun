{-# LANGUAGE TemplateHaskell #-}
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
import Shrun.Data.KeyHide (KeyHide)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Prelude

-- | Holds command logging config.
type CommonLoggingP :: ConfigPhase -> Type
data CommonLoggingP p = MkCommonLoggingP
  { -- | Whether to display command by (key) name or command.
    keyHide :: ConfigPhaseF p KeyHide,
    -- | How to format the timer.
    timerFormat :: ConfigPhaseF p TimerFormat
  }

makeFieldLabelsNoPrefix ''CommonLoggingP

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
        (args ^. #keyHide) <>?. (toml ^. #keyHide),
      timerFormat =
        (args ^. #timerFormat) <>?. (toml ^. #timerFormat)
    }
  where
    toml = fromMaybe defaultToml mToml

instance DecodeTOML CommonLoggingToml where
  tomlDecoder =
    MkCommonLoggingP
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

decodeStripControl :: Decoder (Maybe KeyHide)
decodeStripControl = getFieldOptWith tomlDecoder "key-hide"

decodeCmdLineTrunc :: Decoder (Maybe TimerFormat)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "timer-format"

-- | Creates env version from merged.
toEnv :: CommonLoggingMerged -> CommonLoggingEnv
toEnv merged =
  MkCommonLoggingP
    { keyHide = merged ^. #keyHide,
      timerFormat = merged ^. #timerFormat
    }

defaultToml :: CommonLoggingToml
defaultToml =
  MkCommonLoggingP
    { keyHide = Nothing,
      timerFormat = Nothing
    }

defaultMerged :: CommonLoggingMerged
defaultMerged =
  MkCommonLoggingP
    { keyHide = def,
      timerFormat = def
    }
