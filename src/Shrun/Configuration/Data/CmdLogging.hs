{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CmdLogging
  ( -- * Types
    CmdLoggingP (..),
    CmdLoggingArgs,
    CmdLoggingToml,
    CmdLoggingMerged,
    CmdLoggingEnv,

    -- * Functions
    mergeCmdLogging,
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
import Shrun.Data.CmdLogReadSize (CmdLogReadSize (MkCmdLogReadSize))
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Prelude

-- | Holds config related to (console and file) command logging.
type CmdLoggingP :: ConfigPhase -> Type
data CmdLoggingP p = MkCmdLoggingP
  { -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on cmdLogging or fileLogging since it affects both.
    readSize :: ConfigPhaseF p CmdLogReadSize
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
mergeCmdLogging args mToml =
  MkCmdLoggingP
    { pollInterval =
        (args ^. #pollInterval) <>?. (toml ^. #pollInterval),
      readSize =
        (args ^. #readSize) <>?. (toml ^. #readSize)
    }
  where
    toml = fromMaybe defaultToml mToml

instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingP
      <$> decodePollInterval
      <*> decodeReadSize

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeReadSize :: Decoder (Maybe CmdLogReadSize)
decodeReadSize =
  getFieldOptWith
    (fmap (MkCmdLogReadSize . MkBytes) tomlDecoder)
    "read-size"

-- | Creates env version from merged.
toEnv :: CmdLoggingMerged -> CmdLoggingEnv
toEnv merged =
  MkCmdLoggingP
    { pollInterval = merged ^. #pollInterval,
      readSize = merged ^. #readSize
    }

defaultToml :: CmdLoggingToml
defaultToml =
  MkCmdLoggingP
    { pollInterval = Nothing,
      readSize = Nothing
    }

defaultMerged :: CmdLoggingMerged
defaultMerged =
  MkCmdLoggingP
    { pollInterval = def,
      readSize = def
    }
