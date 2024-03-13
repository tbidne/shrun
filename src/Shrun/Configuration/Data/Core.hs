{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Core
  ( CoreConfigP (..),
    CoreConfigArgs,
    CoreConfigToml,
    CoreConfigMerged,
  )
where

import Shrun.Configuration.Data.CmdLogging (CmdLoggingP)
import Shrun.Configuration.Data.ConfigPhase
import Shrun.Configuration.Data.FileLogging (FileLoggingP)
import Shrun.Configuration.Data.Notify (NotifyP)
import Shrun.Data.KeyHide (KeyHide)
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.Truncation
  ( TruncRegion (TCmdName),
    Truncation,
  )
import Shrun.Prelude

-- | For types that are only guaranteed to exist for Args. Generally this
-- describes "aggregate" types e.g. CmdLoggingP, which always exists for
-- Args (as subfields can independently override toml), but is not
-- guaranteed to exist on toml/merged, since its presence in the latter two
-- indicates active status.
type family ArgsOnlyDetF p a where
  ArgsOnlyDetF ConfigPhaseArgs a = a
  ArgsOnlyDetF ConfigPhaseToml a = Maybe a
  ArgsOnlyDetF ConfigPhaseMerged a = Maybe a

-- | Holds core configuration data.
type CoreConfigP :: ConfigPhase -> Type
data CoreConfigP p = MkCoreConfigP
  { -- | Timeout.
    timeout :: ConfigPhaseMaybeF p Timeout,
    -- | Shell logic to run before each command.
    init :: ConfigPhaseMaybeF p Text,
    -- | Whether to display command by (key) name or command.
    keyHide :: ConfigPhaseF p KeyHide,
    -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on cmdLogging or fileLogging since it affects both.
    cmdLogReadSize :: ConfigPhaseF p (Bytes B Natural),
    -- | How to format the timer.
    timerFormat :: ConfigPhaseF p TimerFormat,
    -- | The max number of command characters to display in the logs.
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    -- | Command log config.
    cmdLogging :: ArgsOnlyDetF p (CmdLoggingP p),
    -- | File log config.
    fileLogging :: ArgsOnlyDetF p (FileLoggingP p),
    -- | Notify config.
    notify :: ArgsOnlyDetF p (NotifyP p)
  }

makeFieldLabelsNoPrefix ''CoreConfigP

type CoreConfigArgs = CoreConfigP ConfigPhaseArgs

type CoreConfigToml = CoreConfigP ConfigPhaseToml

type CoreConfigMerged = CoreConfigP ConfigPhaseMerged

deriving stock instance Eq (CoreConfigP ConfigPhaseArgs)

deriving stock instance Show (CoreConfigP ConfigPhaseArgs)

deriving stock instance Eq (CoreConfigP ConfigPhaseToml)

deriving stock instance Show (CoreConfigP ConfigPhaseToml)

deriving stock instance Eq (CoreConfigP ConfigPhaseMerged)

deriving stock instance Show (CoreConfigP ConfigPhaseMerged)
