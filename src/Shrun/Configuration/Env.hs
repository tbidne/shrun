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

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Shrun (runShellT, shrun)
import Shrun.Command.Types (CommandStatus (CommandWaiting))
import Shrun.Configuration (mergeConfig)
import Shrun.Configuration.Args.Parsing
  ( parserInfoArgs,
  )
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
  )
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

  mTomlConfig <-
    case args ^. #configPath of
      -- 1. If noConfig is true then we ignore all toml config
      Disabled -> pure Nothing
      -- 2. noConfig is false and toml config not set: try reading from
      --    default location. If it does not exist that's fine, just print
      --    a message. If it does, try to read it and throw any errors
      --    (e.g. file errors, toml errors).
      Without -> do
        configDir <- getShrunXdgConfig
        let path = configDir </> [osp|config.toml|]
        b <- doesFileExist path
        if b
          then Just <$> readConfig path
          else do
            putTextLn
              ( "No default config found at: '"
                  <> T.pack (decodeLenient path)
                  <> "'"
              )
            pure Nothing
      -- 3. noConfig is false and toml config explicitly set: try reading
      --    (all errors rethrown)
      With f -> readConfig f

  mergeConfig args mTomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwM tomlErr
{-# INLINEABLE getMergedConfig #-}

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
