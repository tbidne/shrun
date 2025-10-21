{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functions for creating 'Env' from CLI/Toml configuration.
module Shrun.Configuration.Env
  ( -- * Running with Env
    withEnv,
    makeEnvAndShrun,

    -- * Misc
    getMergedConfig,
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Shrun (runShellT, shrun)
import Shrun.Command.Types (CommandStatus (CommandWaiting))
import Shrun.Configuration (mergeConfig)
import Shrun.Configuration.Args.Parsing
  ( parserInfoArgs,
  )
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Configuration.Env.Types
  ( Env
      ( MkEnv,
        anyError,
        commandGraph,
        commands,
        completedCommands,
        config,
        consoleLogQueue,
        timerRegion
      ),
    HasConsoleLogging,
  )
import Shrun.Configuration.Toml (Toml)
import Shrun.Configuration.Toml qualified as Toml
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Notify.DBus (MonadDBus)
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall m r.
  ( HasCallStack,
    HasConsoleLogging (Env r) (Region (ShellT (Env r) m)),
    MonadAsync m,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixSignals m,
    MonadProcess m,
    MonadMask m,
    MonadMVar m,
    MonadSTM m,
    MonadRegionLogger m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
makeEnvAndShrun = withEnv @m @r (runShellT shrun)
{-# INLINEABLE makeEnvAndShrun #-}

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  forall m r a.
  ( HasCallStack,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadThrow m,
    MonadTerminal m
  ) =>
  (Env r -> m a) ->
  m a
withEnv onEnv = getMergedConfig >>= flip fromMergedConfig onEnv
{-# INLINEABLE withEnv #-}

-- | Creates a 'MergedConfig' from CLI args and TOML config.
getMergedConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadThrow m,
    MonadTerminal m
  ) =>
  m MergedConfig
getMergedConfig = do
  args <- execParser parserInfoArgs

  let configPaths = args ^. #configPaths

  tomls <- do
    -- If our configs list contains /any/ Disabled, then the xdg config
    -- will be ignored, as it is the implicit first element. Hence we guard
    -- against it to save an unnecessary lookup.
    if containsDisabled configPaths
      then pure configPaths
      else do
        configDir <- getShrunXdgConfig
        let path = configDir </> [osp|config.toml|]
        b <- doesFileExist path
        -- If xdg config exists, add it to the list. Otherwise just use args.
        if b
          then pure (With path :<| configPaths)
          else do
            putTextLn
              ( "No default config found at: '"
                  <> T.pack (decodeLenient path)
                  <> "'"
              )
            pure configPaths

  mergeTomls tomls >>= mergeConfig args
  where
    containsDisabled = L.elem Disabled
{-# INLINEABLE getMergedConfig #-}

-- | Merges several toml files together.
--
-- NOTE: [Toml order]
--
-- @
--   xdg, t1, t2, ..., tn
-- @
--
-- Where the xdg is the first toml (if it exists), and the rest are given
-- on the CLI. We want the semantics to favor the RHS when there are conflicts.
-- In particular, if some @tk == disabled@, we want all @ti, i < k@ to be
-- disabled.
--
-- Hence we reverse the list to
--
-- @
-- tn, ..., t2, t1, xdg
-- @
--
-- Then drop everything after finding a disabled config. Note that we do
-- /not/ restore the original order (i.e. reverse again). Why? Because our
-- semigroups are left-biased, and we want @tk@ to override @ti@ whenever
-- @i < k@. Hence we can leave the reverse order and foldr.
mergeTomls ::
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  Seq (WithDisabled OsPath) ->
  m Toml
mergeTomls =
  fmap Toml.mergeTomls
    . traverse readConfig
    . dropAfterDisabled
    . Seq.reverse
  where
    dropAfterDisabled Empty = Empty
    dropAfterDisabled (Disabled :<| _) = Empty
    dropAfterDisabled (With f :<| fs) = f :<| dropAfterDisabled fs

readConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Toml
readConfig fp = do
  contents <- readFileUtf8ThrowM fp
  case decode contents of
    Right cfg -> pure cfg
    Left tomlErr -> throwM tomlErr

fromMergedConfig ::
  ( HasCallStack,
    MonadDBus m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  MergedConfig ->
  (Env r -> m a) ->
  m a
fromMergedConfig cfg onEnv = do
  completedCommands <- newTVarA (Map.fromList $ toList commandStatusInit)

  anyError <- newTVarA False
  consoleLogQueue <- newTBQueueA 1_000
  timerRegion <- newIORef Nothing

  CoreConfig.withCoreEnv (cfg ^. #coreConfig) $ \coreConfigEnv -> do
    let env =
          MkEnv
            { config = coreConfigEnv,
              anyError,
              completedCommands,
              consoleLogQueue,
              commandGraph,
              commands,
              timerRegion
            }

    onEnv env
  where
    commands = cfg ^. #commands
    commandGraph = cfg ^. #commandGraph

    commandStatusInit =
      commands <&> \c ->
        ( c ^. #index,
          (c, CommandWaiting ())
        )
{-# INLINEABLE fromMergedConfig #-}

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]
{-# INLINEABLE getShrunXdgConfig #-}
