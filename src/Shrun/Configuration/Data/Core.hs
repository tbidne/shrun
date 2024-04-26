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
import Shrun.Configuration.Data.CommonLogging (CommonLoggingP)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingP)
import Shrun.Configuration.Data.FileLogging (FileLoggingP)
import Shrun.Configuration.Data.Notify (NotifyP)
import Shrun.Data.Timeout (Timeout)
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

type family TomlOptF p a where
  TomlOptF ConfigPhaseArgs a = a
  TomlOptF ConfigPhaseToml a = Maybe a
  TomlOptF ConfigPhaseMerged a = a

-- | Holds core configuration data.
type CoreConfigP :: ConfigPhase -> Type
data CoreConfigP p = MkCoreConfigP
  { -- | Timeout.
    timeout :: ConfigPhaseMaybeF p Timeout,
    -- | Shell logic to run before each command.
    init :: ConfigPhaseMaybeF p Text,
    -- | Holds common logging config.
    commonLogging :: TomlOptF p (CommonLoggingP p),
    -- | Command log config.
    cmdLogging :: TomlOptF p (CmdLoggingP p),
    -- | Holds console logging config.
    consoleLogging :: TomlOptF p (ConsoleLoggingP p),
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
