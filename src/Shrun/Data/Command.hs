{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
module Shrun.Data.Command
  ( CommandP (..),
    CommandP1,
    CommandP2,
    commandToProcess,
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Effects.Process.Typed (ProcessConfig)
import Effects.Process.Typed qualified as P
import Shrun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

data CommandPhase
  = CommandPhase1
  | CommandPhase2

-- | Wrapper for shell commands.
type CommandP :: CommandPhase -> Type
data CommandP p = MkCommandP
  { -- | The key name for the command, for display purposes.
    getKey :: Maybe Text,
    -- | The shell command to run.
    command :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

makeFieldLabelsNoPrefix ''CommandP

instance IsString (CommandP CommandPhase1) where
  fromString = MkCommandP Nothing . T.pack

-- | Phase1 commands.
type CommandP1 = CommandP CommandPhase1

-- | Phase2 commands.
type CommandP2 = CommandP CommandPhase2

advancePhase :: CommandP1 -> Maybe Text -> CommandP2
advancePhase cmd minit = over' #command f cmd
  where
    f = case minit of
      Nothing -> id
      Just init -> \c -> init <> " && " <> c

-- | Transforms a command into its text to be executed by the shell.
commandToShell :: CommandP2 -> String
commandToShell = T.unpack . view #command

-- Transforms a command into a 'ProcessConfig'.
--
commandToProcess :: CommandP1 -> Maybe Text -> ProcessConfig () () ()
commandToProcess command =
  P.shell
    . commandToShell
    . advancePhase command
