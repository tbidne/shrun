{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingP (..),
    CmdLoggingArgs,
    CmdLoggingToml,
    CmdLoggingMerged,
    CmdLoggingEnv,
    mergeCmdLogging,
    toEnv,
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
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.PollInterval (PollInterval, defaultPollInterval)
import Shrun.Logging.Types (defaultCmdLogReadSize)
import Shrun.Prelude

-- | Holds config related to (console and file) command logging.
type CmdLoggingP :: ConfigPhase -> Type
data CmdLoggingP p = MkCmdLoggingP
  { -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on cmdLogging or fileLogging since it affects both.
    readSize :: ConfigPhaseF p (Bytes B Natural)
  }

makeFieldLabelsNoPrefix ''CmdLoggingP

type CmdLoggingArgs = CmdLoggingP ConfigPhaseArgs

type CmdLoggingToml = CmdLoggingP ConfigPhaseToml

type CmdLoggingMerged = CmdLoggingP ConfigPhaseMerged

type CmdLoggingEnv = CmdLoggingP ConfigPhaseEnv

deriving stock instance Eq (CmdLoggingP ConfigPhaseArgs)

deriving stock instance Show (CmdLoggingP ConfigPhaseArgs)

deriving stock instance Eq (CmdLoggingP ConfigPhaseToml)

deriving stock instance Show (CmdLoggingP ConfigPhaseToml)

deriving stock instance Eq (CmdLoggingP ConfigPhaseMerged)

deriving stock instance Show (CmdLoggingP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeCmdLogging ::
  CmdLoggingArgs ->
  Maybe CmdLoggingToml ->
  CmdLoggingMerged
mergeCmdLogging args = \case
  Nothing ->
    MkCmdLoggingP
      { pollInterval =
          WD.fromWithDisabled
            defaultPollInterval
            (args ^. #pollInterval),
        readSize =
          WD.fromWithDisabled
            defaultCmdLogReadSize
            (args ^. #readSize)
      }
  Just toml ->
    MkCmdLoggingP
      { pollInterval =
          plusDefault
            defaultPollInterval
            #pollInterval
            (toml ^. #pollInterval),
        readSize =
          plusDefault
            defaultCmdLogReadSize
            #readSize
            (toml ^. #readSize)
      }
  where
    plusDefault :: a -> Lens' CmdLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingP
      <$> decodePollInterval
      <*> decodeReadSize

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeReadSize :: Decoder (Maybe (Bytes B Natural))
decodeReadSize = getFieldOptWith (fmap MkBytes tomlDecoder) "read-size"

-- | Creates env version from merged.
toEnv :: CmdLoggingMerged -> CmdLoggingEnv
toEnv merged =
  MkCmdLoggingP
    { pollInterval = merged ^. #pollInterval,
      readSize = merged ^. #readSize
    }
