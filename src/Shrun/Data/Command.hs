{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
--
-- @since 0.1
module Shrun.Data.Command
  ( Command (..),
    CommandP1,
    CommandP2,
    commandToProcess,
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text qualified as T
import Effects.System.Process (ProcessConfig)
import Effects.System.Process qualified as P
import Shrun.Data.Phase (AdvancePhase (..), Phase (..))
import Shrun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

-- | Wrapper for shell commands.
--
-- @since 0.1
type Command :: Phase -> Type
data Command p = MkCommand
  { -- | The key name for the command, for display purposes.
    --
    -- @since 0.1
    getKey :: Maybe Text,
    -- | The shell command to run.
    --
    -- @since 0.1
    command :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Command

instance IsString (Command Phase1) where
  fromString = MkCommand Nothing . T.pack

-- | Phase1 commands.
--
-- @since 0.8
type CommandP1 = Command Phase1

-- | Phase2 commands.
--
-- @since 0.8
type CommandP2 = Command Phase2

-- | @since 0.1
instance AdvancePhase (Command Phase1) where
  type NextPhase (Command Phase1) = (Maybe Text -> Command Phase2)

  advancePhase (MkCommand k cmd) Nothing = MkCommand k cmd
  advancePhase (MkCommand k cmd) (Just init) =
    MkCommand k (init <> " && " <> cmd)

-- | Transforms a command into its text to be executed by the shell.
--
-- @since 0.8
commandToShell :: Command Phase2 -> String
commandToShell = T.unpack . view #command

-- Transforms a command into a 'ProcessConfig'.
--
-- @since 0.8
commandToProcess :: Command Phase1 -> Maybe Text -> ProcessConfig () () ()
commandToProcess command =
  P.shell
    . commandToShell
    . advancePhase command
