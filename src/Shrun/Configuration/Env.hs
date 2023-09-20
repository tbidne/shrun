{-# LANGUAGE QuasiQuotes #-}
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
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.FileSystem.Utils qualified as FsUtils
import Effectful.Terminal.Dynamic qualified as Term
import Shrun (shrun)
import Shrun.Configuration.Args
  ( FileMode (FileModeAppend, FileModeWrite),
    FileSizeMode (FileSizeModeDelete, FileSizeModeWarn),
    parserInfoArgs,
  )
import Shrun.Configuration.Env.Notify qualified as EnvNotify
import Shrun.Configuration.Env.Types
  ( CmdLogging (MkCmdLogging, lineTrunc, stripControl),
    Env
      ( MkEnv,
        anyError,
        commands,
        completedCmds,
        init,
        logging,
        notifyEnv,
        timeout
      ),
    FileLogging (MkFileLogging, log, stripControl),
    KeyHide (KeyHideOff),
    LineTruncation (Detected, Undetected),
    Logging
      ( MkLogging,
        cmdLog,
        cmdNameTrunc,
        consoleLog,
        fileLog,
        keyHide,
        pollInterval,
        timerFormat
      ),
    StripControl (StripControlAll, StripControlSmart),
    Truncation (MkTruncation),
  )
import Shrun.Configuration.Legend (linesToMap, translateCommands)
import Shrun.Configuration.Toml
  ( TomlConfig,
    defaultTomlConfig,
    mergeConfig,
  )
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Data.TimerFormat (defaultTimerFormat)
import Shrun.Logging.RegionLogger (RegionLoggerDynamic)
import Shrun.Logging.Types (FileLog, LogRegion)
import Shrun.Notify (runNotifyDynamic)
import Shrun.Notify.AppleScript (AppleScriptDynamic)
import Shrun.Notify.DBus (DBusDynamic)
import Shrun.Notify.NotifySend (NotifySendDynamic)
import Shrun.Prelude

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall es.
  ( AppleScriptDynamic :> es,
    Concurrent :> es,
    DBusDynamic :> es,
    FileReaderStatic :> es,
    FileWriterStatic :> es,
    HandleReaderStatic :> es,
    HandleWriterStatic :> es,
    IORefStatic :> es,
    NotifySendDynamic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    PathWriterStatic :> es,
    RegionLoggerDynamic ConsoleRegion :> es,
    TerminalDynamic :> es,
    TimeDynamic :> es,
    TypedProcess :> es
  ) =>
  Eff es ()
makeEnvAndShrun = withEnv run
  where
    run :: Env -> Eff es ()
    run env =
      runReader env
        $ runNotifyDynamic
        $ shrun @Env @ConsoleRegion

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  ( Concurrent :> es,
    DBusDynamic :> es,
    FileReaderStatic :> es,
    FileWriterStatic :> es,
    HandleWriterStatic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    PathWriterStatic :> es,
    TerminalDynamic :> es
  ) =>
  (Env -> Eff es a) ->
  Eff es a
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
          let path = configDir </> [osp|config.toml|]
          b <- doesFileExist path
          if b
            then readConfig path
            else do
              putTextLn ("No default config found at: " <> FsUtils.decodeOsToFpShowText path)
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
  ( Concurrent :> es,
    DBusDynamic :> es,
    FileWriterStatic :> es,
    HandleWriterStatic :> es,
    PathReaderDynamic :> es,
    PathWriterStatic :> es,
    TerminalDynamic :> es
  ) =>
  TomlConfig ->
  NESeq Text ->
  (Env -> Eff es a) ->
  Eff es a
fromToml cfg cmdsText onEnv = do
  cmdLogLineTrunc <- case cfg ^? (#cmdLog %? #lineTrunc % _Just) of
    Just Detected -> Just . MkTruncation <$> Term.getTerminalWidth
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
                  timerFormat = fromMaybe defaultTimerFormat (cfg ^? (#timerFormat % _Just)),
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
  forall es a.
  ( Concurrent :> es,
    FileWriterStatic :> es,
    HandleWriterStatic :> es,
    PathReaderDynamic :> es,
    PathWriterStatic :> es,
    TerminalDynamic :> es
  ) =>
  TomlConfig ->
  (MLogging -> Eff es a) ->
  Eff es a
withMLogging cfg onLogging = case cfg ^? (#fileLog %? #path) of
  -- 1. No file logging
  Nothing -> onLogging Nothing
  -- 2. Use the default path.
  Just FPDefault -> do
    stateDir <- getShrunXdgState
    let fp = stateDir </> [osp|log|]
    stateExists <- doesDirectoryExist stateDir
    unless stateExists (PW.createDirectoryIfMissing True stateDir)

    ensureFileExists fp
    handleLogFileSize cfg fp

    fileQueue <- newTBQueueA 1000

    HW.withBinaryFile fp ioMode $ \h -> onLogging (Just (h, fileQueue))

  -- 3. Use the given path.
  Just (FPManual fp) -> do
    ensureFileExists fp
    handleLogFileSize cfg fp
    fileQueue <- newTBQueueA 1000

    HW.withBinaryFile fp ioMode $ \h -> onLogging (Just (h, fileQueue))
  where
    ioMode = case fromMaybe FileModeWrite (cfg ^? (#fileLog %? #mode % _Just)) of
      FileModeAppend -> AppendMode
      FileModeWrite -> WriteMode

handleLogFileSize ::
  ( PathReaderDynamic :> es,
    PathWriterStatic :> es,
    TerminalDynamic :> es
  ) =>
  TomlConfig ->
  OsPath ->
  Eff es ()
handleLogFileSize cfg fp = for_ mfileSizeMode $ \fileSizeMode -> do
  fileSize <- MkBytes @B . fromIntegral <$> getFileSize fp
  case fileSizeMode of
    FileSizeModeWarn warnSize ->
      when (fileSize > warnSize)
        $ putTextLn
        $ sizeWarning warnSize fileSize
    FileSizeModeDelete delSize ->
      when (fileSize > delSize) $ do
        putTextLn $ sizeWarning delSize fileSize <> " Deleting log."
        removeFile fp
  where
    mfileSizeMode = cfg ^? (#fileLog %? #sizeMode % _Just)

    sizeWarning warnSize fileSize =
      mconcat
        [ "Warning: log file '",
          FsUtils.decodeOsToFpShowText fp,
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
  ( FileWriterStatic :> es,
    PathReaderDynamic :> es
  ) =>
  OsPath ->
  Eff es ()
ensureFileExists fp = do
  exists <- doesFileExist fp
  unless exists $ writeFileUtf8 fp ""

getShrunXdgConfig :: (PathReaderDynamic :> es) => Eff es OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]

getShrunXdgState :: (PathReaderDynamic :> es) => Eff es OsPath
getShrunXdgState = getXdgState [osp|shrun|]
