{-# LANGUAGE UndecidableInstances #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1
module Shrun.Configuration.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    TruncRegion (..),
    StripControl (..),

    -- * Functions
    withEnv,
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
import Effects.System.MonadTerminal (getTerminalWidth)
import Shrun (runShellT, shrun)
import Shrun.Configuration.Args
  ( FileMode (..),
    FileSizeMode (..),
    parserInfoArgs,
  )
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    Env (..),
    FileLogging (..),
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),
    LineTruncation (..),
    Logging (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Configuration.Legend (linesToMap, translateCommands)
import Shrun.Configuration.Toml (TomlConfig, defaultTomlConfig, mergeConfig)
import Shrun.Data.Command (Command (..))
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | 'withEnv' with 'shrun'.
--
-- @since 0.1
makeEnvAndShrun ::
  ( HasLogging Env (Region (ShellT Env m)),
    MonadCallStack m,
    MonadAsync m,
    MonadExit m,
    MonadFileReader m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
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
--
-- @since 0.5
withEnv ::
  ( MonadCallStack m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
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

  fromToml onEnv finalConfig (args ^. #commands)
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO tomlErr

fromToml ::
  ( MonadFileReader m,
    MonadHandleWriter m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  (Env -> m a) ->
  TomlConfig ->
  NESeq Text ->
  m a
fromToml onEnv cfg cmdsText = do
  cmdLogLineTrunc <- case cfg ^? (#cmdLogging %? #lineTrunc % _Just) of
    Just Detected -> Just . MkTruncation <$> getTerminalWidth
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let fileLogStripControl =
        fromMaybe
          StripControlAll
          (cfg ^? (#fileLogging %? #stripControl % _Just))

  commands' <- case cfg ^. #legend of
    Nothing -> pure $ MkCommand Nothing <$> cmdsText
    Just aliases -> case linesToMap aliases of
      Right mp -> case translateCommands mp cmdsText of
        Right cmds -> pure cmds
        Left err -> throwIO err
      Left err -> throwIO err

  completedCmds' <- newTVarM Seq.empty
  anyError <- newTVarM False

  let envWithLogging mFileLogging consoleLogging =
        MkEnv
          { timeout = cfg ^. #timeout,
            logging =
              MkLogging
                { cmdDisplay = maybeOrMempty cfg (#cmdDisplay % _Just),
                  cmdNameTrunc = cfg ^. #cmdNameTrunc,
                  cmdLogging = case cfg ^. #cmdLogging of
                    Nothing -> Nothing
                    Just cmdLogging ->
                      Just
                        MkCmdLogging
                          { stripControl = maybeOrMempty cmdLogging (#stripControl % _Just),
                            lineTrunc = cmdLogLineTrunc
                          },
                  consoleLogging,
                  fileLogging = case mFileLogging of
                    Nothing -> Nothing
                    Just log ->
                      Just
                        MkFileLogging
                          { log,
                            stripControl = fileLogStripControl
                          }
                },
            anyError,
            completedCmds = completedCmds',
            commands = commands'
          }

      ioMode = case maybeOrMempty cfg (#fileLogging %? #mode % _Just) of
        FileModeAppend -> AppendMode
        FileModeWrite -> WriteMode

  consoleQueue <- newTBQueueM 1000
  case cfg ^? (#fileLogging %? #path) of
    Nothing -> onEnv (envWithLogging Nothing consoleQueue)
    Just FPDefault -> do
      configDir <- getShrunXdgConfig
      let fp = configDir </> "log"
      handleLogFileSize fp

      fileQueue <- newTBQueueM 1000

      bracket (openBinaryFile fp ioMode) closeFile $ \h ->
        onEnv (envWithLogging (Just (h, fileQueue)) consoleQueue)
    Just (FPManual fp) -> do
      handleLogFileSize fp
      fileQueue <- newTBQueueM 1000
      bracket (openBinaryFile fp ioMode) closeFile $ \h ->
        onEnv (envWithLogging (Just (h, fileQueue)) consoleQueue)
  where
    maybeOrMempty :: (Is k An_AffineFold, Monoid a) => s -> Optic' k is s a -> a
    maybeOrMempty x = fromMaybe mempty . (`preview` x)

    handleLogFileSize fp = case cfg ^? (#fileLogging %? #sizeMode % _Just) of
      Nothing -> pure ()
      Just fileSizeMode -> do
        exists <- doesFileExist fp
        when exists $ do
          fileSize <- MkBytes @B . fromIntegral <$> getFileSize fp
          case fileSizeMode of
            FileSizeModeWarn warnSize ->
              when (fileSize > warnSize) $
                putTextLn $
                  sizeWarning warnSize fp fileSize
            FileSizeModeDelete delSize ->
              when (fileSize > delSize) $ do
                putTextLn $ sizeWarning delSize fp fileSize <> " Deleting log."
                removeFile fp

    sizeWarning warnSize fp fileSize =
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

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m FilePath
getShrunXdgConfig = getXdgConfig "shrun"

closeFile :: MonadHandleWriter f => Handle -> f ()
closeFile f = hFlush f *> hClose f
