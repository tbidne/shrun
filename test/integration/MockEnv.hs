module MockEnv (MockEnv (..), defaultEnv) where

import Data.Text (Text)
import ShellRun.Class.Has (HasCommands (..), HasLegend (..))

data MockEnv = MkMockEnv
  { legend :: Maybe Text,
    commands :: [Text]
  }

instance HasLegend MockEnv where
  getLegend = legend

instance HasCommands MockEnv where
  getCommands = commands

defaultEnv :: MockEnv
defaultEnv =
  MkMockEnv
    { legend = Nothing,
      commands = []
    }
