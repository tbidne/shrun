{-# LANGUAGE OverloadedLists #-}
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

import Shrun.Command.Types (CommandP1)
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
    ConfigPhaseDisabledMaybeF,
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingP, mergeConsoleLogging)
import Shrun.Configuration.Data.ConsoleLogging qualified as ConsoleLogging
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingP, mergeFileLogging)
import Shrun.Configuration.Data.FileLogging qualified as FileLogging
import Shrun.Configuration.Data.LegendKeysCache (LegendKeysCache)
import Shrun.Configuration.Data.Notify (NotifyP, mergeNotifyLogging)
import Shrun.Configuration.Data.Notify qualified as Notify
import Shrun.Configuration.Data.Truncation (DetectResult (DetectNotRun))
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<|?|>))
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Notify.DBus (MonadDBus)
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

type family TimeoutF a where
  TimeoutF ConfigPhaseArgs = Maybe (WithDisabled Timeout)
  TimeoutF ConfigPhaseToml = Maybe (WithDisabled Timeout)
  TimeoutF ConfigPhaseMerged = WithDisabled Timeout
  TimeoutF ConfigPhaseEnv = WithDisabled Timeout

type family LegendKeysCacheF a where
  LegendKeysCacheF ConfigPhaseArgs = Maybe LegendKeysCache
  LegendKeysCacheF ConfigPhaseToml = Maybe LegendKeysCache
  LegendKeysCacheF ConfigPhaseMerged = LegendKeysCache
  LegendKeysCacheF ConfigPhaseEnv = ()

-- | Holds core configuration data.
type CoreConfigP :: ConfigPhase -> Type
data CoreConfigP p = MkCoreConfigP
  { -- | Shell logic to run before each command.
    init :: ConfigPhaseDisabledMaybeF p Text,
    -- | Whether to save legend keys.
    legendKeysCache :: LegendKeysCacheF p,
    -- | Timeout.
    timeout :: TimeoutF p,
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
    a ~ ConfigPhaseDisabledMaybeF p Text,
    b ~ ConfigPhaseDisabledMaybeF p Text
  ) =>
  LabelOptic "init" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP b a2 a3 a4 a5 a6 a7 a8)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LegendKeysCacheF p,
    b ~ LegendKeysCacheF p
  ) =>
  LabelOptic "legendKeysCache" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 b a3 a4 a5 a6 a7 a8)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TimeoutF p,
    b ~ TimeoutF p
  ) =>
  LabelOptic "timeout" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 b a4 a5 a6 a7 a8)
          (f a3)
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
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 b a5 a6 a7 a8)
          (f a4)
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
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 b a6 a7 a8)
          (f a5)
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
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 a5 b a7 a8)
          (f a6)
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
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 a5 a6 b a8)
          (f a7)
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
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 a8) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 a5 a6 a7 b)
          (f a8)
  {-# INLINE labelOptic #-}

instance Semigroup CoreConfigToml where
  l <> r =
    MkCoreConfigP
      { init = l ^. #init <|> r ^. #init,
        legendKeysCache = l ^. #legendKeysCache <|> r ^. #legendKeysCache,
        timeout = l ^. #timeout <|> r ^. #timeout,
        commonLogging = l ^. #commonLogging <> r ^. #commonLogging,
        commandLogging = l ^. #commandLogging <> r ^. #commandLogging,
        consoleLogging = l ^. #consoleLogging <> r ^. #consoleLogging,
        fileLogging = l ^. #fileLogging <> r ^. #fileLogging,
        notify = l ^. #notify <> r ^. #notify
      }

instance Monoid CoreConfigToml where
  mempty =
    MkCoreConfigP
      { init = Nothing,
        legendKeysCache = Nothing,
        timeout = Nothing,
        commonLogging = Nothing,
        commandLogging = Nothing,
        consoleLogging = Nothing,
        fileLogging = Nothing,
        notify = Nothing
      }

instance Pretty CoreConfigMerged where
  pretty c =
    vcat
      . toList @Seq
      $ [ "init: " <> prettyMaybe (c ^. #init),
          "legend-keys-cache: " <> pretty (c ^. #legendKeysCache),
          "timeout: " <> pretty (c ^. #timeout),
          "common-logging:",
          indentField $ pretty (c ^. #commonLogging)
        ]
      <> prettyCommandLogging
      <> ["console-logging:", indentField $ pretty (c ^. #consoleLogging)]
      <> prettyMField "file-logging:" (c ^. #fileLogging)
      <> prettyMField "notify:" (c ^. #notify)
    where
      -- If command logging is not on then there is no reason to print its
      -- config.
      prettyCommandLogging =
        if commandLoggingOn
          then ["command-logging:", indentField $ pretty (c ^. #commandLogging)]
          else ["command-logging: off"]

      commandLoggingOn =
        view (#consoleLogging % #commandLogging % #unConsoleLogCmdSwitch) c
          || is (#fileLogging % _Just) c

      prettyMField k Nothing = [k <> " off"]
      prettyMField k (Just x) = [k, indentField (pretty x)]

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
    MonadCatch m,
    MonadIORef m,
    MonadTerminal m
  ) =>
  NESeq CommandP1 ->
  CoreConfigArgs ->
  CoreConfigToml ->
  m CoreConfigMerged
mergeCoreConfig cmds args toml = do
  detectRef <- newIORef DetectNotRun

  consoleLogging <-
    mergeConsoleLogging
      detectRef
      (args ^. #consoleLogging)
      (toml ^. #consoleLogging)

  fileLogging <-
    mergeFileLogging
      detectRef
      (args ^. #fileLogging)
      (toml ^. #fileLogging)

  commandLogging <-
    mergeCommandLogging
      (is _Just fileLogging)
      cmds
      (args ^. #commandLogging)
      (toml ^. #commandLogging)

  pure
    $ MkCoreConfigP
      { timeout = (args ^. #timeout) <.> (toml ^. #timeout),
        legendKeysCache = (args ^. #legendKeysCache) <.> (toml ^. #legendKeysCache),
        init = (args ^. #init) <|?|> (toml ^. #init),
        commonLogging =
          mergeCommonLogging
            (args ^. #commonLogging)
            (toml ^. #commonLogging),
        consoleLogging,
        commandLogging,
        fileLogging,
        notify =
          mergeNotifyLogging
            (args ^. #notify)
            (toml ^. #notify)
      }
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
  CoreConfigMerged ->
  (CoreConfigEnv -> m a) ->
  m a
withCoreEnv merged onCoreConfigEnv = do
  notify <- traverse Notify.toEnv (merged ^. #notify)

  FileLogging.withFileLoggingEnv (merged ^. #fileLogging) $ \fileLoggingEnv -> do
    let coreConfigEnv =
          MkCoreConfigP
            { init = merged ^. #init,
              legendKeysCache = (),
              timeout = merged ^. #timeout,
              commonLogging = CommonLogging.toEnv (merged ^. #commonLogging),
              commandLogging = CommandLogging.toEnv (merged ^. #commandLogging),
              consoleLogging = ConsoleLogging.toEnv (merged ^. #consoleLogging),
              fileLogging = fileLoggingEnv,
              notify
            }
     in onCoreConfigEnv coreConfigEnv
{-# INLINEABLE withCoreEnv #-}

instance Default (CoreConfigP ConfigPhaseArgs) where
  def =
    MkCoreConfigP
      { init = Nothing,
        legendKeysCache = Nothing,
        timeout = Nothing,
        commonLogging = def,
        commandLogging = def,
        consoleLogging = def,
        fileLogging = def,
        notify = def
      }
