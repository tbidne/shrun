{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1
module Shrun.Configuration.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    TruncRegion (..),
    StripControl (..),
    TomlError (..),

    -- * Functions
    withEnv,
    makeEnvAndShrun,
  )
where

import Data.Bytes
  ( FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    formatSized,
    sizedFormatterNatural,
  )
import Data.Sequence qualified as Seq
import Shrun
import Shrun.Configuration.Args (FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    Env (..),
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
    LineTruncation (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Configuration.Legend (linesToMap, translateCommands)
import Shrun.Configuration.Toml (TomlConfig, argsToTomlConfig)
import Shrun.Data.Command (Command (..))
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Effects.FileSystemReader (FileSystemReader (..), getShrunXdgConfig)
import Shrun.Effects.FileSystemWriter (FileSystemWriter (..))
import Shrun.Effects.Mutable (Mutable (..))
import Shrun.Effects.Terminal (Terminal (..))
import Shrun.Effects.Timing (Timing)
import Shrun.Logging.Queue (LogTextQueue (..))
import Shrun.Prelude

-- | @since 0.5
newtype TomlError = MkTomlError
  { -- | @since 0.5
    unTomlError :: TOMLError
  }
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
makePrisms ''TomlError

-- | @since 0.5
instance Exception TomlError where
  displayException err =
    "TOML error: " <> unpack (renderTOMLError $ err ^. _MkTomlError)

-- | 'withEnv' with 'shrun'.
--
-- @since 0.1
makeEnvAndShrun ::
  ( FileSystemReader m,
    FileSystemWriter m,
    MonadMask m,
    MonadUnliftIO m,
    Mutable m,
    Terminal m,
    Timing m
  ) =>
  m ()
makeEnvAndShrun = withEnv (runShellT shrun)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
--
-- @since 0.5
withEnv ::
  ( FileSystemReader m,
    FileSystemWriter m,
    MonadUnliftIO m,
    Mutable m,
    Terminal m
  ) =>
  (Env -> m a) ->
  m a
withEnv onEnv = do
  args <- getArgs
  tomlConfig <-
    if args ^. #noConfig
      then -- 1. If noConfig is true then we ignore all toml config
        pure mempty
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
              pure mempty

  let finalConfig = args ^. argsToTomlConfig <> tomlConfig

  fromToml onEnv finalConfig (args ^. #commands)
  where
    readConfig fp = do
      contents <- readFile fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkTomlError tomlErr

fromToml ::
  ( FileSystemReader m,
    FileSystemWriter m,
    MonadUnliftIO m,
    Mutable m,
    Terminal m
  ) =>
  (Env -> m a) ->
  TomlConfig ->
  NonEmptySeq Text ->
  m a
fromToml onEnv cfg cmdsText = do
  cmdLineTrunc' <- case cfg ^. #cmdLineTrunc of
    Just Detected -> Just . MkTruncation <$> getTerminalWidth
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let disableLogging' = fromMaybe False (cfg ^. #disableLogging)
      fileLogStripControl = fromMaybe StripControlAll (cfg ^. #fileLogStripControl)

  commands' <- case cfg ^. #legend of
    Nothing -> pure $ MkCommand Nothing <$> cmdsText
    Just aliases -> case linesToMap aliases of
      Right mp -> case translateCommands mp cmdsText of
        Right cmds -> pure cmds
        Left err -> throwIO err
      Left err -> throwIO err

  completedCmds' <- newTVarIO Seq.empty

  let envWithFileLogging fl =
        MkEnv
          { timeout = cfg ^. #timeout,
            fileLog = fl,
            fileLogStripControl = fileLogStripControl,
            cmdLogging = maybeOrMempty #cmdLogging,
            cmdDisplay = maybeOrMempty #cmdDisplay,
            cmdNameTrunc = cfg ^. #cmdNameTrunc,
            cmdLineTrunc = cmdLineTrunc',
            stripControl = maybeOrMempty #stripControl,
            completedCmds = completedCmds',
            disableLogging = disableLogging',
            commands = commands'
          }

      ioMode = case maybeOrMempty #fileLogMode of
        FileModeAppend -> AppendMode
        FileModeWrite -> WriteMode

  case cfg ^. #fileLog of
    Nothing -> onEnv (envWithFileLogging Nothing)
    Just FPDefault -> do
      configDir <- getShrunXdgConfig
      let fp = configDir </> "log"
      handleLogFileSize fp

      queue <- liftSTM $ newTBQueue 1000

      withFile fp ioMode $ \h ->
        onEnv (envWithFileLogging (Just (h, MkLogTextQueue queue)))
    Just (FPManual fp) -> do
      handleLogFileSize fp
      queue <- liftSTM $ newTBQueue 1000
      withFile fp ioMode $ \h ->
        onEnv (envWithFileLogging (Just (h, MkLogTextQueue queue)))
  where
    maybeOrMempty :: Monoid a => Lens' TomlConfig (Maybe a) -> a
    maybeOrMempty = fromMaybe mempty . (`view` cfg)

    handleLogFileSize fp = case cfg ^. #fileLogSizeMode of
      Nothing -> pure ()
      Just fileSizeMode -> do
        fileSize <- getFileSize fp
        case fileSizeMode of
          FileSizeModeWarn warnSize ->
            when (fileSize > warnSize) $
              putTextLn $
                sizeWarning warnSize fp fileSize
          FileSizeModeDelete delSize ->
            when (fileSize > delSize) $ do
              putTextLn $ sizeWarning delSize fp fileSize <> " Deleting log."
              deleteFile fp

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
