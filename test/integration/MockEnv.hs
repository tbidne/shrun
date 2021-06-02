-- | Provides 'MockEnv' type for running integration tests.
module MockEnv (MockEnv (..), defaultEnv) where

import Data.Text (Text)
import ShellRun.Class.Has (HasCommands (..), HasLegend (..))

-- | Includes the bare minimum fields necessary to run 'ShellRun.runShell'.
data MockEnv = MkMockEnv
  { legend :: Maybe Text,
    commands :: [Text]
  }

instance HasLegend MockEnv where
  getLegend = legend

instance HasCommands MockEnv where
  getCommands = commands

-- | Constructs a default 'MockEnv'.
defaultEnv :: MockEnv
defaultEnv =
  MkMockEnv
    { legend = Nothing,
      commands = []
    }
