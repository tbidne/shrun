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
  )
where

import Shrun.Configuration.Data.CommandLogging (CommandLoggingP, mergeCommandLogging)
import Shrun.Configuration.Data.CommandLogging qualified as CommandLogging
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
import Shrun.Configuration.Default (Default (def))
import Shrun.Data.Command (CommandP1)
import Shrun.Notify.MonadDBus (MonadDBus)
import Shrun.Prelude

-- | For types that are only guaranteed to exist for Args. Generally this
-- describes "aggregate" types e.g. CommandLoggingP, which always exists for
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
    commandLogging :: TomlOptF p (CommandLoggingP p),
    -- | Holds console logging config.
    consoleLogging :: TomlOptF p (ConsoleLoggingP p),
    -- | File log config.
    fileLogging :: ArgsOnlyDetF p (FileLoggingP p),
    -- | Notify config.
    notify :: ArgsOnlyDetF p (NotifyP p)
  }

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseMaybeF p Text,
    b ~ ConfigPhaseMaybeF p Text
  ) =>
  LabelOptic "init" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \init' ->
                MkCoreConfigP
                  init'
                  _timeout
                  _commonLogging
                  _commandLogging
                  _consoleLogging
                  _fileLogging
                  _notify
            )
            (f _init)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseMaybeF p Timeout,
    b ~ ConfigPhaseMaybeF p Timeout
  ) =>
  LabelOptic "timeout" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \timeout' ->
                MkCoreConfigP
                  _init
                  timeout'
                  _commonLogging
                  _commandLogging
                  _consoleLogging
                  _fileLogging
                  _notify
            )
            (f _timeout)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (CommonLoggingP p),
    b ~ TomlOptF p (CommonLoggingP p)
  ) =>
  LabelOptic "commonLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \commonLogging' ->
                MkCoreConfigP
                  _init
                  _timeout
                  commonLogging'
                  _commandLogging
                  _consoleLogging
                  _fileLogging
                  _notify
            )
            (f _commonLogging)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (CommandLoggingP p),
    b ~ TomlOptF p (CommandLoggingP p)
  ) =>
  LabelOptic "commandLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \commandLogging' ->
                MkCoreConfigP
                  _init
                  _timeout
                  _commonLogging
                  commandLogging'
                  _consoleLogging
                  _fileLogging
                  _notify
            )
            (f _commandLogging)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (ConsoleLoggingP p),
    b ~ TomlOptF p (ConsoleLoggingP p)
  ) =>
  LabelOptic "consoleLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \consoleLogging' ->
                MkCoreConfigP
                  _init
                  _timeout
                  _commonLogging
                  _commandLogging
                  consoleLogging'
                  _fileLogging
                  _notify
            )
            (f _consoleLogging)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ArgsOnlyDetF p (FileLoggingP p),
    b ~ ArgsOnlyDetF p (FileLoggingP p)
  ) =>
  LabelOptic "fileLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( \fileLogging' ->
                MkCoreConfigP
                  _init
                  _timeout
                  _commonLogging
                  _commandLogging
                  _consoleLogging
                  fileLogging'
                  _notify
            )
            (f _fileLogging)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ArgsOnlyDetF p (NotifyP p),
    b ~ ArgsOnlyDetF p (NotifyP p)
  ) =>
  LabelOptic "notify" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCoreConfigP
             _init
             _timeout
             _commonLogging
             _commandLogging
             _consoleLogging
             _fileLogging
             _notify
           ) ->
          fmap
            ( MkCoreConfigP
                _init
                _timeout
                _commonLogging
                _commandLogging
                _consoleLogging
                _fileLogging
            )
            (f _notify)
  {-# INLINE labelOptic #-}

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
        commandLogging =
          mergeCommandLogging
            (args ^. #commandLogging)
            (toml ^. #commandLogging),
        fileLogging,
        notify =
          mergeNotifyLogging
            (args ^. #notify)
            (toml ^. #notify)
      }
  where
    toml = fromMaybe def mToml
{-# INLINEABLE mergeCoreConfig #-}

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
  NESeq CommandP1 ->
  CoreConfigMerged ->
  (CoreConfigEnv -> m a) ->
  m a
withCoreEnv cmds merged onCoreConfigEnv = do
  notify <- traverse Notify.toEnv (merged ^. #notify)
  commandLogging <- CommandLogging.toEnv cmds (merged ^. #commandLogging)

  FileLogging.withFileLoggingEnv (merged ^. #fileLogging) $ \fileLoggingEnv ->
    let coreConfigEnv =
          MkCoreConfigP
            { init = merged ^. #init,
              timeout = merged ^. #timeout,
              commonLogging = CommonLogging.toEnv (merged ^. #commonLogging),
              commandLogging,
              consoleLogging = ConsoleLogging.toEnv (merged ^. #consoleLogging),
              fileLogging = fileLoggingEnv,
              notify
            }
     in onCoreConfigEnv coreConfigEnv
{-# INLINEABLE withCoreEnv #-}

instance
  ( Default (ConfigPhaseMaybeF p Text),
    Default (ConfigPhaseMaybeF p Timeout),
    Default (TomlOptF p (CommonLoggingP p)),
    Default (TomlOptF p (CommandLoggingP p)),
    Default (TomlOptF p (ConsoleLoggingP p)),
    Default (ArgsOnlyDetF p (FileLoggingP p)),
    Default (ArgsOnlyDetF p (NotifyP p))
  ) =>
  Default (CoreConfigP p)
  where
  def =
    MkCoreConfigP
      { init = def,
        timeout = def,
        commonLogging = def,
        commandLogging = def,
        consoleLogging = def,
        fileLogging = def,
        notify = def
      }
