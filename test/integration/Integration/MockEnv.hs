{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'MockEnv' type for running integration tests.
module Integration.MockEnv (MockEnv (..), defaultEnv) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Integration.Prelude
import ShellRun.Command (Command)
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env
  ( HasCommands (..),
    HasCompletedCmds (..),
    HasLegend (..),
    HasLogging (..),
  )
import ShellRun.Env.Types (CmdDisplay (..), CmdLogging (..), HasTimeout (..), StripControl (..), Truncation (..))

-- | Includes the bare minimum fields necessary to run 'ShellRun.runShell'.
data MockEnv = MkMockEnv
  { legend :: FilePathDefault,
    commands :: NonEmptySeq Text,
    completedCmds :: TVar (Seq Command),
    logs :: TVar [Text]
  }

makeFieldLabelsNoPrefix ''MockEnv

instance HasLegend MockEnv where
  getLegend = view #legend

instance HasCommands MockEnv where
  getCommands = view #commands

instance HasCompletedCmds MockEnv where
  getCompletedCmds = view #completedCmds

instance HasLogging MockEnv where
  getCmdDisplay _ = ShowKey
  getCmdLineTrunc _ = MkTruncation PPosInf
  getCmdLogging _ = Enabled
  getCmdNameTrunc _ = MkTruncation PPosInf
  getFileLogging _ = Nothing
  getGlobalLogging _ = True
  getStripControl _ = StripControlNone

instance HasTimeout MockEnv where
  getTimeout _ = MkTimeout PPosInf

-- | Constructs a default 'MockEnv'.
defaultEnv :: NonEmptySeq Text -> IO MockEnv
defaultEnv cmds = do
  completedCmds' <- newTVarIO Seq.empty
  logs' <- newTVarIO []
  pure $
    MkMockEnv
      { legend = FPNone,
        commands = cmds,
        completedCmds = completedCmds',
        logs = logs'
      }
