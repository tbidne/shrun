{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP (..),
    CommonLoggingArgs,
    CommonLoggingToml,
    CommonLoggingMerged,
    mergeCommonLogging,
    defaultCommonLoggingMerged,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.KeyHide (KeyHide, defaultKeyHide)
import Shrun.Data.TimerFormat (TimerFormat, defaultTimerFormat)
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
mergeCommonLogging args = \case
  Nothing ->
    MkCommonLoggingP
      { keyHide =
          WD.fromWithDisabled
            defaultKeyHide
            (args ^. #keyHide),
        timerFormat =
          WD.fromWithDisabled
            defaultTimerFormat
            (args ^. #timerFormat)
      }
  Just toml ->
    MkCommonLoggingP
      { keyHide =
          plusDefault
            defaultKeyHide
            #keyHide
            (toml ^. #keyHide),
        timerFormat =
          plusDefault
            defaultTimerFormat
            #timerFormat
            (toml ^. #timerFormat)
      }
  where
    plusDefault :: a -> Lens' CommonLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

instance DecodeTOML CommonLoggingToml where
  tomlDecoder =
    MkCommonLoggingP
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

decodeStripControl :: Decoder (Maybe KeyHide)
decodeStripControl = getFieldOptWith tomlDecoder "key-hide"

decodeCmdLineTrunc :: Decoder (Maybe TimerFormat)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "timer-format"

defaultCommonLoggingMerged :: CommonLoggingMerged
defaultCommonLoggingMerged =
  MkCommonLoggingP
    { keyHide = defaultKeyHide,
      timerFormat = defaultTimerFormat
    }
