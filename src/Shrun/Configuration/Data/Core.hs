{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Core
  ( CoreConfigP (..),
    CoreConfigArgs,
    CoreConfigToml,
    CoreConfigMerged,
    mergeCoreConfig,
  )
where

import Shrun.Configuration.Data.CmdLogging (CmdLoggingP, mergeCmdLogging)
import Shrun.Configuration.Data.CommonLogging (CommonLoggingP, mergeCommonLogging)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingP, mergeConsoleLogging)
import Shrun.Configuration.Data.FileLogging (FileLoggingP, mergeFileLogging)
import Shrun.Configuration.Data.Notify (NotifyP, mergeNotifyLogging)
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?))
import Shrun.Configuration.Data.WithDisabled qualified as WD
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
  { -- | Shell logic to run before each command.
    init :: ConfigPhaseMaybeF p Text,
    -- | Timeout.
    timeout :: ConfigPhaseMaybeF p Timeout,
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

mergeCoreConfig ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  CoreConfigArgs ->
  Maybe CoreConfigToml ->
  m CoreConfigMerged
mergeCoreConfig args = \case
  Nothing -> do
    consoleLogging <-
      mergeConsoleLogging
        (args ^. #consoleLogging)
        Nothing

    pure
      $ MkCoreConfigP
        { timeout = WD.toMaybe (args ^. #timeout),
          init = WD.toMaybe (args ^. #init),
          commonLogging =
            mergeCommonLogging
              (args ^. #commonLogging)
              Nothing,
          consoleLogging,
          cmdLogging =
            mergeCmdLogging
              (args ^. #cmdLogging)
              Nothing,
          fileLogging =
            mergeFileLogging
              (args ^. #fileLogging)
              Nothing,
          notify =
            mergeNotifyLogging
              (args ^. #notify)
              Nothing
        }
  Just toml -> do
    consoleLogging <-
      mergeConsoleLogging
        (args ^. #consoleLogging)
        (toml ^. #consoleLogging)

    pure
      $ MkCoreConfigP
        { timeout =
            plusNothing #timeout (toml ^. #timeout),
          init =
            plusNothing #init (toml ^. #init),
          commonLogging =
            mergeCommonLogging
              (args ^. #commonLogging)
              (toml ^. #commonLogging),
          consoleLogging,
          cmdLogging =
            mergeCmdLogging
              (args ^. #cmdLogging)
              (toml ^. #cmdLogging),
          fileLogging =
            mergeFileLogging
              (args ^. #fileLogging)
              (toml ^. #fileLogging),
          notify =
            mergeNotifyLogging
              (args ^. #notify)
              (toml ^. #notify)
        }
  where
    plusNothing :: Lens' CoreConfigArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. l) <>? r
