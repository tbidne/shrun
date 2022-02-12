-- | Provides 'MockEnv' type for running integration tests.
module MockEnv (MockEnv (..), defaultEnv) where

import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Prelude

-- | Includes the bare minimum fields necessary to run 'ShellRun.runShell'.
data MockEnv = MkMockEnv
  { legend :: Maybe FilePath,
    commands :: List Text
  }

instance HasLegend MockEnv where
  getLegend :: MockEnv -> Maybe FilePath
  getLegend = legend

instance HasCommands MockEnv where
  getCommands :: MockEnv -> List Text
  getCommands = commands

-- | Constructs a default 'MockEnv'.
defaultEnv :: MockEnv
defaultEnv =
  MkMockEnv
    { legend = Nothing,
      commands = []
    }
