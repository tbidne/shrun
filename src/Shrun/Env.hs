{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functions for creating 'Env' from CLI/Toml configuration.
module Shrun.Env
  ( -- * Running with Env
    withEnv,
    makeEnvAndShrun,

    -- * Misc
    getMergedConfig,
  )
where

import Data.Bytes
  ( FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    formatSized,
    sizedFormatterNatural,
  )
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.HandleWriter (withBinaryFile)
import Effects.FileSystem.PathWriter
  ( MonadPathWriter
      ( createDirectoryIfMissing
      ),
  )
import Effects.FileSystem.Utils qualified as FsUtils
import GHC.Num (Num (fromInteger))
import Shrun (runShellT, shrun)
import Shrun.Configuration (mergeConfig)
import Shrun.Configuration.Args.Parsing
  ( parserInfoArgs,
  )
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
  )
import Shrun.Data.FileMode (FileMode (FileModeAppend, FileModeWrite))
import Shrun.Data.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Data.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeNothing,
        FileSizeModeWarn
      ),
  )
import Shrun.Data.StripControl (StripControl)
import Shrun.Env.Notify qualified as EnvNotify
import Shrun.Env.Types
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
    HasLogging,
    Logging
      ( MkLogging,
        cmdLog,
        cmdLogReadSize,
        cmdNameTrunc,
        consoleLog,
        fileLog,
        keyHide,
        pollInterval,
        timerFormat
      ),
  )
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types (FileLog)
import Shrun.Notify.MonadAppleScript (MonadAppleScript)
import Shrun.Notify.MonadDBus (MonadDBus)
import Shrun.Notify.MonadNotifySend (MonadNotifySend)
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
    MonadTypedProcess m,
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
  (Env -> m a) ->
  m a
fromMergedConfig cfg onEnv = do
  completedCmds <- newTVarA Seq.empty
  anyError <- newTVarA False
  consoleLog <- newTBQueueA 1000

  notifyEnv <- EnvNotify.tomlToNotifyEnv (cfg ^. (#coreConfig % #notify))

  let -- make environment
      envWithLogging ::
        -- optional file logging
        Maybe (Tuple3 Handle (TBQueue FileLog) StripControl) ->
        Env
      envWithLogging mFileLogging =
        MkEnv
          { timeout = cfg ^. (#coreConfig % #timeout),
            init = cfg ^. (#coreConfig % #init),
            notifyEnv,
            logging =
              MkLogging
                { keyHide = cfg ^. (#coreConfig % #keyHide),
                  pollInterval = cfg ^. (#coreConfig % #pollInterval),
                  timerFormat = cfg ^. (#coreConfig % #timerFormat),
                  cmdNameTrunc = cfg ^. (#coreConfig % #cmdNameTrunc),
                  cmdLogReadSize = cfg ^. (#coreConfig % #cmdLogReadSize),
                  cmdLog =
                    cfg ^. (#coreConfig % #cmdLogging) <&> \cmdLog ->
                      MkCmdLogging
                        { stripControl = cmdLog ^. #stripControl,
                          lineTrunc = cfg ^? (#coreConfig % #cmdLogging %? #lineTrunc % _Just)
                        },
                  consoleLog,
                  fileLog =
                    mFileLogging <&> \(h, q, sc) ->
                      MkFileLogging
                        { log = (h, q),
                          stripControl = sc
                        }
                },
            anyError,
            completedCmds,
            commands = cfg ^. #commands
          }

  withMLogging cfg $ \h -> onEnv (envWithLogging h)

type MLogging = Maybe (Tuple3 Handle (TBQueue FileLog) StripControl)

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
  MergedConfig ->
  (MLogging -> m a) ->
  m a
withMLogging cfg onLogging = case cfg ^. (#coreConfig % #fileLogging) of
  -- 1. No file logging
  Nothing -> onLogging Nothing
  -- 2. Use the default path.
  Just fileLogging -> do
    let ioMode = case fileLogging ^. #mode of
          FileModeAppend -> AppendMode
          FileModeWrite -> WriteMode

    fp <- case fileLogging ^. #path of
      FPDefault -> do
        stateDir <- getShrunXdgState
        let fp = stateDir </> [osp|shrun.log|]
        stateExists <- doesDirectoryExist stateDir
        unless stateExists (createDirectoryIfMissing True stateDir)
        pure fp
      FPManual fp -> pure fp

    ensureFileExists fp
    handleLogFileSize cfg fp
    fileQueue <- newTBQueueA 1000

    withBinaryFile fp ioMode $ \h ->
      onLogging (Just (h, fileQueue, fileLogging ^. #stripControl))

handleLogFileSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  MergedConfig ->
  OsPath ->
  m ()
handleLogFileSize cfg fp = for_ mfileSizeMode $ \fileSizeMode -> do
  fileSize <- MkBytes @B . unsafeConvertIntegral <$> getFileSize fp
  case fileSizeMode of
    FileSizeModeWarn warnSize ->
      when (fileSize > warnSize)
        $ putTextLn
        $ sizeWarning warnSize fileSize
    FileSizeModeDelete delSize ->
      when (fileSize > delSize) $ do
        putTextLn $ sizeWarning delSize fileSize <> " Deleting log."
        removeFile fp
    FileSizeModeNothing -> pure ()
  where
    mfileSizeMode = cfg ^? (#coreConfig % #fileLogging %? #sizeMode)

    sizeWarning warnSize fileSize =
      mconcat
        [ "Warning: log file '",
          T.pack $ FsUtils.decodeOsToFpShow fp,
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
        . fmap (toDouble . unsafeConvertIntegral)

    toDouble :: Integer -> Double
    toDouble = fromInteger

ensureFileExists ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m
  ) =>
  OsPath ->
  m ()
ensureFileExists fp = do
  exists <- doesFileExist fp
  unless exists $ writeFileUtf8 fp ""

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]

getShrunXdgState :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgState = getXdgState [osp|shrun|]
