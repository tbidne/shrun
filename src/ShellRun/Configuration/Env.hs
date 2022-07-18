{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1
module ShellRun.Configuration.Env
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

    -- * Functions
    makeEnv,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Options.Applicative qualified as OApp
import ShellRun.Configuration.Args (parserInfoArgs)
import ShellRun.Configuration.Env.Types
  ( ALineTruncation (..),
    CmdDisplay (..),
    CmdLogging (..),
    Env (..),
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import ShellRun.Configuration.Legend (linesToMap, translateCommands)
import ShellRun.Configuration.Toml (TomlConfig, argsToTomlConfig)
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Prelude
import System.Console.Terminal.Size (Window (..))
import System.Console.Terminal.Size qualified as TSize
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | @since 0.5
data TermSizeException = MkTermSizeException
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
instance Exception TermSizeException where
  displayException = const "Failed to detect the terminal size."

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

-- | Returns the 'Env' based on the CLI args and TOML config file.
-- CLI args take precedence over config file.
--
-- @since 0.1
makeEnv ::
  ( MonadFSReader m,
    MonadUnliftIO m
  ) =>
  m Env
makeEnv = do
  args <- liftIO $ OApp.execParser parserInfoArgs
  fileConfig <- case args ^. #configPath of
    Nothing -> do
      configDir <- getXdgConfig "shell-run"
      let path = configDir </> "config.toml"
      b <- liftIO $ doesFileExist path
      if b
        then readConfig (configDir </> "config.toml")
        else do
          liftIO $ putStrLn ("No default config found at: " <> pack path)
          pure mempty
    Just f -> readConfig f

  let finalConfig = args ^. argsToTomlConfig <> fileConfig

  configToEnv finalConfig (args ^. #commands)
  where
    readConfig fp = do
      contents <- readFile fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkTomlError tomlErr

configToEnv ::
  ( MonadFSReader m,
    MonadIO m
  ) =>
  TomlConfig ->
  NonEmptySeq Text ->
  m Env
configToEnv cfg cmdsText = do
  fileLogging' <- case cfg ^. #fileLogging of
    Nothing -> pure Nothing
    Just FPDefault -> do
      configDir <- getXdgConfig "shell-run"
      let fp = configDir </> "shell-run.log"
      queue <- liftIO $ atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
    Just (FPManual f) -> do
      queue <- liftIO $ atomically $ TBQueue.newTBQueue 1000
      pure $ Just (f, MkLogTextQueue queue)

  cmdLineTrunc' <- case cfg ^. #cmdLineTrunc of
    Just Detected ->
      (width <<$>> liftIO TSize.size) >>= \case
        Just h -> pure $ Just $ MkTruncation h
        Nothing -> throwIO MkTermSizeException
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let disableLogging' = fromMaybe False (cfg ^. #disableLogging)

  commands' <- case cfg ^. #legend of
    Nothing -> pure $ MkCommand Nothing <$> cmdsText
    Just txt -> case linesToMap $ fmap T.strip (T.lines txt) of
      Right mp -> case translateCommands mp cmdsText of
        Right cmds -> pure cmds
        Left err -> throwIO err
      Left err -> throwIO err

  completedCmds' <- liftIO $ newTVarIO Seq.empty

  pure $
    MkEnv
      { timeout = cfg ^. #timeout,
        fileLogging = fileLogging',
        cmdLogging = maybeOrMempty #cmdLogging,
        cmdDisplay = maybeOrMempty #cmdDisplay,
        cmdNameTrunc = cfg ^. #cmdNameTrunc,
        lineNameTrunc = cmdLineTrunc',
        stripControl = maybeOrMempty #stripControl,
        completedCmds = completedCmds',
        disableLogging = disableLogging',
        commands = commands'
      }
  where
    maybeOrMempty :: Monoid a => Lens' TomlConfig (Maybe a) -> a
    maybeOrMempty = fromMaybe mempty . (`view` cfg)
