-- | Provides the 'Command' wrapper for commands.
--
-- @since 0.1.0.0
module ShellRun.Data.Command (Command (..)) where

import ShellRun.Prelude

-- | Wrapper for shell commands.
--
-- @since 0.1.0.0
data Command = MkCommand
  { -- | The key name for the command, for display purposes.
    --
    -- @since 0.1.0.0
    getKey :: Maybe Text,
    -- | The shell command to run.
    --
    -- @since 0.1.0.0
    command :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
