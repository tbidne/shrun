{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
  ( Bytes (MkBytes),
    FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    Size (B),
    formatSized,
    sizedFormatterNatural,
  )
import Data.Sequence qualified as Seq
import Data.Text.Encoding qualified as TEnc
import Effects.MonadFs
  ( MonadFsReader
      ( doesFileExist,
        getFileSize,
        getXdgConfig
      ),
    MonadFsWriter
      ( hClose,
        openFile,
        removeFile
      ),
  )
import Effects.MonadSTM
import Effects.MonadTerminal (getTerminalWidth)
import Effects.MonadTime (MonadTime)
import Options.Applicative qualified as OA
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
makeFieldLabelsNoPrefix ''TomlError

-- | @since 0.5
instance Exception TomlError where
  displayException err =
    "TOML error: " <> unpack (renderTOMLError $ err ^. #unTomlError)

-- | 'withEnv' with 'shrun'.
--
-- @since 0.1
makeEnvAndShrun ::
  ( MonadCallStack m,
    MonadFsReader m,
    MonadFsWriter m,
    MonadIORef m,
    MonadMask m,
    MonadTBQueue m,
    MonadTerminal m,
    MonadTVar m,
    MonadThread m,
    MonadTime m,
    MonadUnliftIO m
  ) =>
  m ()
makeEnvAndShrun = withEnv (runShellT shrun)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
--
-- @since 0.5
withEnv ::
  ( MonadCallStack m,
    MonadFsReader m,
    MonadFsWriter m,
    MonadTBQueue m,
    MonadTerminal m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  (Env -> m a) ->
  m a
withEnv onEnv = do
  args <- liftIO $ OA.execParser parserInfoArgs
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
      contents <-
        readFile fp
          >>= ( \case
                  Left ex -> throwWithCallStack ex
                  Right c -> pure c
              )
            . TEnc.decodeUtf8'
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkTomlError tomlErr

fromToml ::
  ( MonadFsReader m,
    MonadFsWriter m,
    MonadTBQueue m,
    MonadTerminal m,
    MonadTVar m,
    MonadUnliftIO m
  ) =>
  (Env -> m a) ->
  TomlConfig ->
  NonEmptySeq Text ->
  m a
fromToml onEnv cfg cmdsText = do
  cmdLogLineTrunc <- case cfg ^? (#cmdLogging %? #lineTrunc % _Just) of
    Just Detected -> Just . MkTruncation <$> getTerminalWidth
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let disableLogging' = fromMaybe False (cfg ^. #disableLogging)
      fileLogStripControl =
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

  let envWithFileLogging mfl =
        MkEnv
          { timeout = cfg ^. #timeout,
            disableLogging = disableLogging',
            cmdDisplay = maybeOrMempty cfg (#cmdDisplay % _Just),
            cmdNameTrunc = cfg ^. #cmdNameTrunc,
            cmdLogging = case cfg ^. #cmdLogging of
              Nothing -> Nothing
              Just cmdLogging ->
                Just
                  MkCmdLogging
                    { stripControl = maybeOrMempty cmdLogging (#stripControl % _Just),
                      lineTrunc = cmdLogLineTrunc
                    },
            fileLogging = case mfl of
              Nothing -> Nothing
              Just log ->
                Just
                  MkFileLogging
                    { log,
                      stripControl = fileLogStripControl
                    },
            completedCmds = completedCmds',
            commands = commands'
          }

      ioMode = case maybeOrMempty cfg (#fileLogging %? #mode % _Just) of
        FileModeAppend -> AppendMode
        FileModeWrite -> WriteMode

  case cfg ^? (#fileLogging %? #path) of
    Nothing -> onEnv (envWithFileLogging Nothing)
    Just FPDefault -> do
      configDir <- getShrunXdgConfig
      let fp = configDir </> "log"
      handleLogFileSize fp

      queue <- newTBQueueM 1000

      bracket (openFile fp ioMode) closeFile $ \h ->
        onEnv (envWithFileLogging (Just (h, MkLogTextQueue queue)))
    Just (FPManual fp) -> do
      handleLogFileSize fp
      queue <- newTBQueueM 1000
      bracket (openFile fp ioMode) closeFile $ \h ->
        onEnv (envWithFileLogging (Just (h, MkLogTextQueue queue)))
  where
    maybeOrMempty :: (Is k An_AffineFold, Monoid a) => s -> Optic' k is s a -> a
    maybeOrMempty x = fromMaybe mempty . (`preview` x)

    handleLogFileSize fp = case cfg ^? (#fileLogging %? #sizeMode % _Just) of
      Nothing -> pure ()
      Just fileSizeMode -> do
        fileSize <- MkBytes @B <$> getFileSize fp
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

getShrunXdgConfig :: (HasCallStack, MonadFsReader m) => m FilePath
getShrunXdgConfig = getXdgConfig "shrun"

closeFile :: MonadFsWriter f => Handle -> f ()
closeFile f = hFlush f *> hClose f
