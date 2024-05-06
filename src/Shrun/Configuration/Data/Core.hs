{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Core
  ( -- * Types
    CoreConfigP (..),
    CoreConfigArgs,
    CoreConfigToml,
    CoreConfigMerged,

    -- * Functions
    mergeCoreConfig,
    withCoreEnv,

    -- * Misc
    defaultMerged,
  )
where

import Shrun.Configuration.Data.CmdLogging (CmdLoggingP, mergeCmdLogging)
import Shrun.Configuration.Data.CmdLogging qualified as CmdLogging
import Shrun.Configuration.Data.CommonLogging (CommonLoggingP, mergeCommonLogging)
import Shrun.Configuration.Data.CommonLogging qualified as CommonLogging
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingP, mergeConsoleLogging)
import Shrun.Configuration.Data.ConsoleLogging qualified as ConsoleLogging
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingP, mergeFileLogging)
import Shrun.Configuration.Data.FileLogging qualified as FileLogging
import Shrun.Configuration.Data.Notify (NotifyP, mergeNotifyLogging)
import Shrun.Configuration.Data.Notify qualified as Notify
import Shrun.Configuration.Data.WithDisabled ((<>??))
import Shrun.Notify.MonadDBus (MonadDBus)
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
  ArgsOnlyDetF ConfigPhaseEnv a = Maybe a

-- | For types that are optional only on the Toml.
type family TomlOptF p a where
  TomlOptF ConfigPhaseArgs a = a
  TomlOptF ConfigPhaseToml a = Maybe a
  TomlOptF ConfigPhaseMerged a = a
  TomlOptF ConfigPhaseEnv a = a

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

type CoreConfigEnv = CoreConfigP ConfigPhaseEnv

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
mergeCoreConfig args mToml = do
  consoleLogging <-
    mergeConsoleLogging
      (args ^. #consoleLogging)
      (toml ^. #consoleLogging)

  fileLogging <-
    mergeFileLogging
      (args ^. #fileLogging)
      (toml ^. #fileLogging)

  pure
    $ MkCoreConfigP
      { timeout = (args ^. #timeout) <>?? (toml ^. #timeout),
        init = (args ^. #init) <>?? (toml ^. #init),
        commonLogging =
          mergeCommonLogging
            (args ^. #commonLogging)
            (toml ^. #commonLogging),
        consoleLogging,
        cmdLogging =
          mergeCmdLogging
            (args ^. #cmdLogging)
            (toml ^. #cmdLogging),
        fileLogging,
        notify =
          mergeNotifyLogging
            (args ^. #notify)
            (toml ^. #notify)
      }
  where
    toml = fromMaybe defaultToml mToml

-- | Given a merged CoreConfig, constructs a ConfigEnv and calls the
-- continuation.
withCoreEnv ::
  forall m a.
  ( HasCallStack,
    MonadDBus m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  CoreConfigMerged ->
  (CoreConfigEnv -> m a) ->
  m a
withCoreEnv merged onCoreConfigEnv = do
  notify <- traverse Notify.toEnv (merged ^. #notify)

  FileLogging.withFileLoggingEnv (merged ^. #fileLogging) $ \fileLoggingEnv ->
    let coreConfigEnv =
          MkCoreConfigP
            { init = merged ^. #init,
              timeout = merged ^. #timeout,
              commonLogging = CommonLogging.toEnv (merged ^. #commonLogging),
              cmdLogging = CmdLogging.toEnv (merged ^. #cmdLogging),
              consoleLogging = ConsoleLogging.toEnv (merged ^. #consoleLogging),
              fileLogging = fileLoggingEnv,
              notify
            }
     in onCoreConfigEnv coreConfigEnv

defaultToml :: CoreConfigToml
defaultToml =
  MkCoreConfigP
    { init = Nothing,
      timeout = Nothing,
      commonLogging = Nothing,
      cmdLogging = Nothing,
      consoleLogging = Nothing,
      fileLogging = Nothing,
      notify = Nothing
    }

defaultMerged :: CoreConfigMerged
defaultMerged =
  MkCoreConfigP
    { init = Nothing,
      timeout = Nothing,
      commonLogging = CommonLogging.defaultMerged,
      cmdLogging = CmdLogging.defaultMerged,
      consoleLogging = ConsoleLogging.defaultMerged,
      fileLogging = Nothing,
      notify = Nothing
    }
