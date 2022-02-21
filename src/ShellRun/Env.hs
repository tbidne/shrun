{-# LANGUAGE RecordWildCards #-}

-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1.0.0
module ShellRun.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCmdDisplay (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    TruncRegion (..),

    -- * Functions
    runParser,
  )
where

import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Options.Applicative qualified as OApp
import ShellRun.Args (ALineTruncation (..), Args (..), FilePathDefault (..))
import ShellRun.Args qualified as Args
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    Env (..),
    HasCmdDisplay (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasCommands (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),
    TruncRegion (..),
    Truncation (..),
  )
import ShellRun.Logging.Queue (LogTextQueue (..))
import ShellRun.Prelude
import System.Console.Terminal.Size (Window (..))
import System.Console.Terminal.Size qualified as TSize
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir
import System.Exit qualified as SysExit
import System.FilePath ((</>))

-- | Runs the parser.
--
-- @since 0.1.0.0
runParser :: IO Env
runParser = do
  args@MkArgs {aFileLogging} <- OApp.execParser Args.parserInfoArgs

  fileLogging' <- case aFileLogging of
    FPNone -> pure Nothing
    FPDefault -> do
      configDir <- Dir.getXdgDirectory XdgConfig "shell-run"
      let fp = configDir </> "logs.txt"
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
    FPManual fp -> do
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)

  toEnv fileLogging' args

toEnv :: Maybe (Tuple2 FilePath LogTextQueue) -> Args -> IO Env
toEnv fileLogging' MkArgs {..} = do
  lineNameTrunc' <- case aCmdLineTrunc of
    Undetected x -> pure x
    Detected ->
      (width <<$>> TSize.size) >>= \case
        Just h -> pure $ MkTruncation (PFin h)
        Nothing -> SysExit.die "Failed trying to detect terminal size"
  pure $
    MkEnv
      { legend = aLegend,
        timeout = aTimeout,
        fileLogging = fileLogging',
        cmdLogging = aCmdLogging,
        cmdDisplay = aCmdDisplay,
        cmdNameTrunc = aCmdNameTrunc,
        lineNameTrunc = lineNameTrunc',
        commands = aCommands
      }
