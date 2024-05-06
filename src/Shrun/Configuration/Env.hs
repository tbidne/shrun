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

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Shrun (runShellT, shrun)
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
        commands,
        completedCommands,
        config,
        consoleLogQueue
      ),
    HasConsoleLogging,
  )
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Notify.MonadAppleScript (MonadAppleScript)
import Shrun.Notify.MonadDBus (MonadDBus)
import Shrun.Notify.MonadNotifySend (MonadNotifySend)
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall m r.
  ( HasConsoleLogging (Env r) (Region (ShellT (Env r) m)),
    MonadAppleScript m,
    MonadAsync m,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadNotifySend m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTypedProcess m,
    MonadMask m,
    MonadSTM m,
    MonadRegionLogger m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
makeEnvAndShrun = withEnv @m @r (runShellT shrun)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  forall m r a.
  ( MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
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

-- | Creates a 'MergedConfig' from CLI args and TOML config.
getMergedConfig ::
  ( MonadDBus m,
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
              ( "No default config found at: "
                  <> T.pack (FsUtils.decodeOsToFpShow path)
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

fromMergedConfig ::
  ( MonadDBus m,
    MonadFileWriter m,
    MonadHandleWriter m,
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
  completedCommands <- newTVarA Seq.empty
  anyError <- newTVarA False
  consoleLogQueue <- newTBQueueA 1_000

  CoreConfig.withCoreEnv (cfg ^. #coreConfig) $ \coreConfigEnv -> do
    let env =
          MkEnv
            { config = coreConfigEnv,
              anyError,
              completedCommands,
              consoleLogQueue,
              commands = cfg ^. #commands
            }

    onEnv env

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]
