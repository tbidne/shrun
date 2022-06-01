{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'IntEnv' type for running integration tests.
module Integration.IntEnv (IntEnv (..), defaultEnv) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time.Relative (RelativeTime)
import Integration.Prelude
import Numeric.Algebra.Additive.AMonoid (zero)
import ShellRun.Command (Command)
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Effects.MonadProcRunner (MonadProcRunner (..))
import ShellRun.Env
  ( HasCommands (..),
    HasCompletedCmds (..),
    HasLegend (..),
    HasLogging (..),
  )
import ShellRun.Env.Types (CmdDisplay (..), CmdLogging (..), HasTimeout (..), StripControl (..), Truncation (..))
import ShellRun.IO (Stderr)
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.ShellT (ShellT)
import System.Console.Regions (ConsoleRegion)

-- | Includes the bare minimum fields necessary to run 'ShellRun.runShell'.
data IntEnv = MkIntEnv
  { legend :: FilePathDefault,
    commands :: NonEmptySeq Text,
    completedCmds :: TVar (Seq Command),
    logs :: TVar [Text],
    cmdsRun :: TVar [Command]
  }

makeFieldLabelsNoPrefix ''IntEnv

instance HasLegend IntEnv where
  getLegend = view #legend

instance HasCommands IntEnv where
  getCommands = view #commands

instance HasCompletedCmds IntEnv where
  getCompletedCmds = view #completedCmds

instance HasLogging IntEnv where
  getCmdDisplay _ = ShowKey
  getCmdLineTrunc _ = MkTruncation PPosInf
  getCmdLogging _ = Enabled
  getCmdNameTrunc _ = MkTruncation PPosInf
  getFileLogging _ = Nothing
  getGlobalLogging _ = True
  getStripControl _ = StripControlNone

instance HasTimeout IntEnv where
  getTimeout _ = MkTimeout PPosInf

instance RegionLogger (ShellT IntEnv IO) where
  type Region (ShellT IntEnv IO) = ConsoleRegion
  logFn logTxt = do
    ls <- asks $ view #logs
    liftIO $ atomically $ modifyTVar' ls (logTxt :)
  logModeToRegionFn _ _ = logFn

instance MonadProcRunner (ShellT IntEnv IO) where
  tryTimeProc = saveCmd
  tryTimeProcStream = saveCmd
  tryTimeProcStreamRegion = saveCmd

saveCmd :: Command -> ShellT IntEnv IO (Either (RelativeTime, Stderr) RelativeTime)
saveCmd cmd = do
  cr <- asks $ view #cmdsRun
  liftIO $ atomically $ modifyTVar' cr (cmd :)
  pure $ Right zero

-- | Constructs a default 'IntEnv'.
defaultEnv :: NonEmptySeq Text -> IO IntEnv
defaultEnv cmds = do
  completedCmds' <- newTVarIO Seq.empty
  logs' <- newTVarIO []
  cmdsRun' <- newTVarIO []
  pure $
    MkIntEnv
      { legend = FPNone,
        commands = cmds,
        completedCmds = completedCmds',
        logs = logs',
        cmdsRun = cmdsRun'
      }
