{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging
  ( -- * Types
    CommandLoggingP (..),
    CommandLoggingArgs,
    CommandLoggingToml,
    CommandLoggingMerged,
    CommandLoggingEnv,

    -- * Functions
    mergeCommandLogging,
    toEnv,

    -- * Misc
    defaultMerged,
  )
where

import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize (MkReadSize))
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

-- | Holds config related to (console and file) command logging.
type CommandLoggingP :: ConfigPhase -> Type
data CommandLoggingP p = MkCommandLoggingP
  { -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on commandLogging or fileLogging since it affects both.
    readSize :: ConfigPhaseF p ReadSize
  }

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p PollInterval, b ~ ConfigPhaseF p PollInterval) =>
  LabelOptic "pollInterval" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic = lensVL $ \f (MkCommandLoggingP _pollInterval _readSize) ->
    fmap (`MkCommandLoggingP` _readSize) (f _pollInterval)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p ReadSize, b ~ ConfigPhaseF p ReadSize) =>
  LabelOptic "readSize" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic = lensVL $ \f (MkCommandLoggingP _pollInterval _readSize) ->
    fmap (MkCommandLoggingP _pollInterval) (f _readSize)
  {-# INLINE labelOptic #-}

type CommandLoggingArgs = CommandLoggingP ConfigPhaseArgs

type CommandLoggingToml = CommandLoggingP ConfigPhaseToml

type CommandLoggingMerged = CommandLoggingP ConfigPhaseMerged

type CommandLoggingEnv = CommandLoggingP ConfigPhaseEnv

deriving stock instance Eq (CommandLoggingP ConfigPhaseArgs)

deriving stock instance Show (CommandLoggingP ConfigPhaseArgs)

deriving stock instance Eq (CommandLoggingP ConfigPhaseToml)

deriving stock instance Show (CommandLoggingP ConfigPhaseToml)

deriving stock instance Eq (CommandLoggingP ConfigPhaseMerged)

deriving stock instance Show (CommandLoggingP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeCommandLogging ::
  CommandLoggingArgs ->
  Maybe CommandLoggingToml ->
  CommandLoggingMerged
mergeCommandLogging args mToml =
  MkCommandLoggingP
    { pollInterval =
        (args ^. #pollInterval) <>?. (toml ^. #pollInterval),
      readSize =
        (args ^. #readSize) <>?. (toml ^. #readSize)
    }
  where
    toml = fromMaybe defaultToml mToml

instance DecodeTOML CommandLoggingToml where
  tomlDecoder =
    MkCommandLoggingP
      <$> decodePollInterval
      <*> decodeReadSize

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeReadSize :: Decoder (Maybe ReadSize)
decodeReadSize =
  getFieldOptWith
    (fmap (MkReadSize . MkBytes) tomlDecoder)
    "read-size"

-- | Creates env version from merged.
toEnv :: CommandLoggingMerged -> CommandLoggingEnv
toEnv merged =
  MkCommandLoggingP
    { pollInterval = merged ^. #pollInterval,
      readSize = merged ^. #readSize
    }

defaultToml :: CommandLoggingToml
defaultToml =
  MkCommandLoggingP
    { pollInterval = Nothing,
      readSize = Nothing
    }

defaultMerged :: CommandLoggingMerged
defaultMerged =
  MkCommandLoggingP
    { pollInterval = def,
      readSize = def
    }
