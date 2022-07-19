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
    TomlError (..),

    -- * Functions
    makeEnv,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Data.Sequence qualified as Seq
import ShellRun.Configuration.Env.Types
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
import ShellRun.Configuration.Legend (linesToMap, translateCommands)
import ShellRun.Configuration.Toml (TomlConfig, argsToTomlConfig)
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Effects.Atomic (Atomic (..))
import ShellRun.Effects.FileSystemReader (FileSystemReader (..))
import ShellRun.Effects.Terminal (Terminal (..))
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Prelude
import System.FilePath ((</>))

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
  ( Atomic m,
    FileSystemReader m,
    MonadIO m,
    Terminal m
  ) =>
  m Env
makeEnv = do
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
          configDir <- getXdgConfig "shell-run"
          let path = configDir </> "config.toml"
          b <- doesFileExist path
          if b
            then readConfig (configDir </> "config.toml")
            else do
              putTextLn ("No default config found at: " <> pack path)
              pure mempty

  let finalConfig = args ^. argsToTomlConfig <> tomlConfig

  configToEnv finalConfig (args ^. #commands)
  where
    readConfig fp = do
      contents <- readFile fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkTomlError tomlErr

configToEnv ::
  ( Atomic m,
    FileSystemReader m,
    MonadIO m,
    Terminal m
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
      queue <- liftSTM $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
    Just (FPManual f) -> do
      queue <- liftSTM $ TBQueue.newTBQueue 1000
      pure $ Just (f, MkLogTextQueue queue)

  cmdLineTrunc' <- case cfg ^. #cmdLineTrunc of
    Just Detected -> Just . MkTruncation <$> getTerminalWidth
    Just (Undetected x) -> pure $ Just x
    Nothing -> pure Nothing

  let disableLogging' = fromMaybe False (cfg ^. #disableLogging)

  commands' <- case cfg ^. #legend of
    Nothing -> pure $ MkCommand Nothing <$> cmdsText
    Just aliases -> case linesToMap aliases of
      Right mp -> case translateCommands mp cmdsText of
        Right cmds -> pure cmds
        Left err -> throwIO err
      Left err -> throwIO err

  completedCmds' <- newTVarIO Seq.empty

  pure $
    MkEnv
      { timeout = cfg ^. #timeout,
        fileLogging = fileLogging',
        cmdLogging = maybeOrMempty #cmdLogging,
        cmdDisplay = maybeOrMempty #cmdDisplay,
        cmdNameTrunc = cfg ^. #cmdNameTrunc,
        cmdLineTrunc = cmdLineTrunc',
        stripControl = maybeOrMempty #stripControl,
        completedCmds = completedCmds',
        disableLogging = disableLogging',
        commands = commands'
      }
  where
    maybeOrMempty :: Monoid a => Lens' TomlConfig (Maybe a) -> a
    maybeOrMempty = fromMaybe mempty . (`view` cfg)
