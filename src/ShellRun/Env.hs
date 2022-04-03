-- | Module that provides env types and requisite typeclasses, along with
-- parsing functionality.
--
-- @since 0.1
module ShellRun.Env
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCmdDisplay (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasCmdLineTrunc (..),
    HasCompletedCmds (..),
    HasFileLogging (..),
    HasGlobalLogging (..),
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
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad.STM qualified as STM
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Options.Applicative qualified as OApp
import ShellRun.Args (ALineTruncation (..), Args (..))
import ShellRun.Args qualified as Args
import ShellRun.Command (Command)
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    Env (..),
    HasCmdDisplay (..),
    HasCmdLineTrunc (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasCommands (..),
    HasCompletedCmds (..),
    HasFileLogging (..),
    HasGlobalLogging (..),
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
-- @since 0.1
runParser :: IO Env
runParser = do
  args <- OApp.execParser Args.parserInfoArgs

  fileLogging' <- case args ^. #fileLogging of
    FPNone -> pure Nothing
    FPDefault -> do
      configDir <- Dir.getXdgDirectory XdgConfig "shell-run"
      let fp = configDir </> "logs.txt"
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)
    FPManual fp -> do
      queue <- STM.atomically $ TBQueue.newTBQueue 1000
      pure $ Just (fp, MkLogTextQueue queue)

  completedCmds' <- TVar.newTVarIO Seq.empty

  toEnv fileLogging' completedCmds' args

toEnv :: Maybe (Tuple2 FilePath LogTextQueue) -> TVar (Seq Command) -> Args -> IO Env
toEnv fileLogging' completedCmds' args = do
  lineNameTrunc' <- case args ^. #cmdLineTrunc of
    Undetected x -> pure x
    Detected ->
      (width <<$>> TSize.size) >>= \case
        Just h -> pure $ MkTruncation (PFin h)
        Nothing -> SysExit.die "Failed trying to detect terminal size"
  pure $
    MkEnv
      { legend = args ^. #legend,
        timeout = args ^. #timeout,
        fileLogging = fileLogging',
        cmdLogging = args ^. #cmdLogging,
        cmdDisplay = args ^. #cmdDisplay,
        cmdNameTrunc = args ^. #cmdNameTrunc,
        lineNameTrunc = lineNameTrunc',
        completedCmds = completedCmds',
        globalLogging = args ^. #globalLogging,
        commands = args ^. #commands
      }
