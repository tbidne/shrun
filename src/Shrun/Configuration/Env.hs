{-# LANGUAGE UndecidableInstances #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1
module Shrun.Configuration.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    Env.prependCompletedCommand,
    HasLogging (..),
    HasTimeout (..),
    HasAnyError (..),
    Env.setAnyErrorTrue,

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
import Effects.FileSystem.PathWriter (MonadPathWriter (createDirectoryIfMissing))
import Effects.System.Terminal (getTerminalWidth)
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
    HasAnyError (..),
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),
    LineTruncation (..),
    Logging (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Configuration.Env.Types qualified as Env
import Shrun.Configuration.Legend (linesToMap, translateCommands)
import Shrun.Configuration.Toml (TomlConfig, defaultTomlConfig, mergeConfig)
import Shrun.Data.Command (Command (..))
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types (FileLog, LogRegion)
import Shrun.Prelude
import Shrun.ShellT (ShellT)
import Shrun.Utils qualified as U

-- | 'withEnv' with 'shrun'.
--
-- @since 0.1
makeEnvAndShrun ::
  ( HasLogging Env (Region (ShellT Env m)),
    MonadAsync m,
    MonadFileReader m,
    MonadFileWriter m,
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
  ( MonadFileReader m,
    MonadFileWriter m,
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
        Left tomlErr -> throwM tomlErr

fromToml ::
  ( MonadFileReader m,
    MonadFileWriter m,
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
        Left err -> throwM err
      Left err -> throwM err

  completedCmds' <- newTVarM Seq.empty
  anyError <- newTVarM False

  -- get functions that need to be run with bracket
  let (acquireFileLogging, closeFileLogging) = fileLoggingBracketFns cfg

      -- make environment
      envWithLogging ::
        -- optional file logging
        Maybe (Tuple2 Handle (TBQueue FileLog)) ->
        -- console logging
        TBQueue (LogRegion ConsoleRegion) ->
        Env
      envWithLogging mFileLogging consoleLogging =
        MkEnv
          { timeout = cfg ^. #timeout,
            shellInit = cfg ^. #shellInit,
            logging =
              MkLogging
                { cmdDisplay = fromMaybe ShowKey (cfg ^? (#cmdDisplay % _Just)),
                  pollInterval = fromMaybe defaultPollInterval (cfg ^? (#pollInterval % _Just)),
                  cmdNameTrunc = cfg ^. #cmdNameTrunc,
                  cmdLogging =
                    cfg ^. #cmdLogging <&> \cmdLogging ->
                      MkCmdLogging
                        { stripControl =
                            fromMaybe StripControlSmart (cmdLogging ^? (#stripControl % _Just)),
                          lineTrunc = cmdLogLineTrunc
                        },
                  consoleLogging,
                  fileLogging =
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

  consoleQueue <- newTBQueueM 1000

  bracket
    acquireFileLogging
    closeFileLogging
    (\mLogging -> onEnv (envWithLogging mLogging consoleQueue))

type BracketFns m a = Tuple2 (m a) (a -> m ())

-- Return (acquire, cleanup) functions for file logging
fileLoggingBracketFns ::
  forall m.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  TomlConfig ->
  BracketFns m (Maybe (Tuple2 Handle (TBQueue FileLog)))
fileLoggingBracketFns cfg = (acquireFileLogging, closeFileLogging)
  where
    acquireFileLogging :: m (Maybe (Tuple2 Handle (TBQueue FileLog)))
    closeFileLogging :: Maybe (Tuple2 Handle (TBQueue FileLog)) -> m ()

    (acquireFileLogging, closeFileLogging) = case cfg ^? (#fileLogging %? #path) of
      -- 1. No file logging: Do nothing
      Nothing -> (pure Nothing, \_ -> pure ())
      -- 2. Use the default path.
      --      Acquire: Create the dirs/queue, open the file.
      --      Cleanup: Close the file.
      Just FPDefault ->
        let acquire = do
              stateDir <- getShrunXdgState
              let fp = stateDir </> "log"
              stateExists <- doesDirectoryExist stateDir
              unless stateExists (createDirectoryIfMissing True stateDir)

              ensureFileExists fp
              handleLogFileSize cfg fp

              fileQueue <- newTBQueueM 1000

              Just . (,fileQueue) <$> openBinaryFile fp ioMode

            cleanup = flip U.whenJust (closeFile . fst)
         in (acquire, cleanup)
      -- 3. Use the given path.
      --      Acquire: Create the queue, open the file.
      --      Cleanup: Close the file.
      Just (FPManual fp) ->
        let acquire = do
              ensureFileExists fp
              handleLogFileSize cfg fp
              fileQueue <- newTBQueueM 1000

              Just . (,fileQueue) <$> openBinaryFile fp ioMode

            cleanup = flip U.whenJust (closeFile . fst)
         in (acquire, cleanup)

    ioMode = case fromMaybe FileModeWrite (cfg ^? (#fileLogging %? #mode % _Just)) of
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
handleLogFileSize cfg fp = U.whenJust mfileSizeMode $ \fileSizeMode -> do
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
    mfileSizeMode = cfg ^? (#fileLogging %? #sizeMode % _Just)

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

closeFile :: (MonadHandleWriter f) => Handle -> f ()
closeFile f = hFlush f *> hClose f
