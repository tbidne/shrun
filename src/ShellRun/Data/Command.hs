-- | Provides the 'Command' wrapper for commands.
module ShellRun.Data.Command (Command (..)) where

import Data.Text (Text)

-- | Wrapper for shell commands.
data Command = MkCommand
  { -- | The key name for the command, for display purposes
    getKey :: Maybe Text,
    -- | The shell command to run
    command :: Text
  }
  deriving (Eq, Show)
