{-# LANGUAGE UndecidableInstances #-}

-- | Provides functions for creating 'Env' from CLI/Toml configuration.
module Shrun.Configuration.Env
  ( withEnv,
    makeEnvAndShrun,
  )
where

import Data.Bytes
  ( Bytes (MkBytes),
    FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    Size (B),
    formatSized,
    sizedFormatterNatural,
  )
import Data.Sequence qualified as Seq
import Effects.FileSystem.HandleWriter (withBinaryFile)
import Effects.FileSystem.PathWriter (MonadPathWriter (createDirectoryIfMissing))
import Effects.System.Terminal (getTerminalWidth)
import Shrun (runShellT, shrun)
import Shrun.Configuration.Args
  ( FileMode (..),
    FileSizeMode (..),
    parserInfoArgs,
  )
import Shrun.Configuration.Env.Notify qualified as EnvNotify
import Shrun.Configuration.Env.Types
  ( CmdLogging (..),
    Env (..),
    FileLogging (..),
    HasLogging (..),
    KeyHide (..),
    LineTruncation (..),
    Logging (..),
    StripControl (..),
    Truncation (..),
  )
import Shrun.Configuration.Legend (linesToMap, translateCommands)
import Shrun.Configuration.Toml
  ( TomlConfig,
    defaultTomlConfig,
    mergeConfig,
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types (FileLog, LogRegion)
import Shrun.Notify.MonadAppleScript (MonadAppleScript)
import Shrun.Notify.MonadDBus (MonadDBus (..))
import Shrun.Notify.MonadNotifySend (MonadNotifySend (..))
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  ( HasLogging Env (Region (ShellT Env m)),
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
    MonadProcess m,
    MonadMask m,
    MonadSTM m,
    MonadRegionLogger m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
makeEnvAndShrun = withEnv (runShellT shrun)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
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
  (Env -> m a) ->
  m a
withEnv onEnv = do
  args <- execParser parserInfoArgs
  tomlConfig <-
    if args ^. #noConfig
      then -- 1. If noConfig is true then we ignore all toml config
        pure defaultTomlConfig
      else case args ^. #configPath of
        -- 2. noConfig is false and toml config explicitly set: try reading
        --    (all errors rethrown)
        Just f -> readConfig f
        -- 3. noConfig is false and toml config not set: try reading from
        --    default location. If it does not exist that's fine, just print
        --    a message. If it does, try to read it and throw any errors
        --    (e.g. file errors, toml errors).
        Nothing -> do
          configDir <- getShrunXdgConfig
          let path = configDir </> "config.toml"
          b <- doesFileExist path
          if b
            then readConfig path
            else do
              putTextLn ("No default config found at: " <> pack path)
              pure defaultTomlConfig

  let finalConfig = mergeConfig args tomlConfig

  fromToml finalConfig (args ^. #commands) onEnv
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwM tomlErr

fromToml ::
  ( MonadDBus m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  TomlConfig ->
  NESeq Text ->
  (Env -> m a) ->
  m a
fromToml cfg cmdsText onEnv = do
  cmdLogLineTrunc <- case cfg ^? (#cmdLog %? #lineTrunc % _Just) of
    Just Detected -> Just . MkTruncation <$> getTerminalWidth
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let fileLogStripControl =
        fromMaybe
          StripControlAll
          (cfg ^? (#fileLog %? #stripControl % _Just))

  commands' <- case cfg ^. #legend of
    Nothing -> pure $ MkCommand Nothing <$> cmdsText
    Just aliases -> case linesToMap aliases of
      Right mp -> case translateCommands mp cmdsText of
        Right cmds -> pure cmds
        Left err -> throwM err
      Left err -> throwM err

  completedCmds' <- newTVarA Seq.empty
  anyError <- newTVarA False

  notifyEnv <- EnvNotify.tomlToNotifyEnv (cfg ^. #notify)

  let -- make environment
      envWithLogging ::
        -- optional file logging
        Maybe (Tuple2 Handle (TBQueue FileLog)) ->
        -- console logging
        TBQueue (LogRegion ConsoleRegion) ->
        Env
      envWithLogging mFileLogging consoleLog =
        MkEnv
          { timeout = cfg ^. #timeout,
            init = cfg ^. #init,
            notifyEnv,
            logging =
              MkLogging
                { keyHide = fromMaybe KeyHideOff (cfg ^? (#keyHide % _Just)),
                  pollInterval = fromMaybe defaultPollInterval (cfg ^? (#pollInterval % _Just)),
                  cmdNameTrunc = cfg ^. #cmdNameTrunc,
                  cmdLog =
                    cfg ^. #cmdLog <&> \cmdLog ->
                      MkCmdLogging
                        { stripControl =
                            fromMaybe StripControlSmart (cmdLog ^? (#stripControl % _Just)),
                          lineTrunc = cmdLogLineTrunc
                        },
                  consoleLog,
                  fileLog =
                    mFileLogging <&> \log ->
                      MkFileLogging
                        { log,
                          stripControl = fileLogStripControl
                        }
                },
            anyError,
            completedCmds = completedCmds',
            commands = commands'
          }

  consoleQueue <- newTBQueueA 1000

  withMLogging cfg $ \h -> onEnv (envWithLogging h consoleQueue)

type MLogging = Maybe (Tuple2 Handle (TBQueue FileLog))

withMLogging ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  TomlConfig ->
  (MLogging -> m a) ->
  m a
withMLogging cfg onLogging = case cfg ^? (#fileLog %? #path) of
  -- 1. No file logging
  Nothing -> onLogging Nothing
  -- 2. Use the default path.
  Just FPDefault -> do
    stateDir <- getShrunXdgState
    let fp = stateDir </> "log"
    stateExists <- doesDirectoryExist stateDir
    unless stateExists (createDirectoryIfMissing True stateDir)

    ensureFileExists fp
    handleLogFileSize cfg fp

    fileQueue <- newTBQueueA 1000

    withBinaryFile fp ioMode $ \h -> onLogging (Just (h, fileQueue))

  -- 3. Use the given path.
  Just (FPManual fp) -> do
    ensureFileExists fp
    handleLogFileSize cfg fp
    fileQueue <- newTBQueueA 1000

    withBinaryFile fp ioMode $ \h -> onLogging (Just (h, fileQueue))
  where
    ioMode = case fromMaybe FileModeWrite (cfg ^? (#fileLog %? #mode % _Just)) of
      FileModeAppend -> AppendMode
      FileModeWrite -> WriteMode

handleLogFileSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  TomlConfig ->
  FilePath ->
  m ()
handleLogFileSize cfg fp = for_ mfileSizeMode $ \fileSizeMode -> do
  fileSize <- MkBytes @B . fromIntegral <$> getFileSize fp
  case fileSizeMode of
    FileSizeModeWarn warnSize ->
      when (fileSize > warnSize) $
        putTextLn $
          sizeWarning warnSize fileSize
    FileSizeModeDelete delSize ->
      when (fileSize > delSize) $ do
        putTextLn $ sizeWarning delSize fileSize <> " Deleting log."
        removeFile fp
  where
    mfileSizeMode = cfg ^? (#fileLog %? #sizeMode % _Just)

    sizeWarning warnSize fileSize =
      mconcat
        [ "Warning: log file '",
          pack fp,
          "' has size: ",
          formatBytes fileSize,
          ", but specified threshold is: ",
          formatBytes warnSize,
          "."
        ]

    formatBytes =
      formatSized (MkFloatingFormatter (Just 2)) sizedFormatterNatural
        . normalize
        -- Convert to double _before_ normalizing. We may lose some precision
        -- here, but it is better than normalizing a natural, which will
        -- truncate (i.e. greater precision loss).
        . fmap (fromIntegral @Natural @Double)

ensureFileExists ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m
  ) =>
  FilePath ->
  m ()
ensureFileExists fp = do
  exists <- doesFileExist fp
  unless exists $ writeFileUtf8 fp ""

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m FilePath
getShrunXdgConfig = getXdgConfig "shrun"

getShrunXdgState :: (HasCallStack, MonadPathReader m) => m FilePath
getShrunXdgState = getXdgState "shrun"
