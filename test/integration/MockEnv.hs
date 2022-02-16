-- | Provides 'MockEnv' type for running integration tests.
module MockEnv (MockEnv (..), defaultEnv) where

import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Prelude

-- | Includes the bare minimum fields necessary to run 'ShellRun.runShell'.
data MockEnv = MkMockEnv
  { legend :: Maybe FilePath,
    commands :: NonEmptySeq Text
  }

instance HasLegend MockEnv where
  getLegend = legend

instance HasCommands MockEnv where
  getCommands = commands

-- | Constructs a default 'MockEnv'.
defaultEnv :: NonEmptySeq Text -> MockEnv
defaultEnv cmds =
  MkMockEnv
    { legend = Nothing,
      commands = cmds
    }
