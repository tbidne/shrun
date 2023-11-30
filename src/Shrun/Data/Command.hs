{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
module Shrun.Data.Command
  ( Command (..),
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
import Shrun.Data.Phase
  ( AdvancePhase (NextPhase, advancePhase),
    Phase (Phase1, Phase2),
  )
import Shrun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

-- | Wrapper for shell commands.
type Command :: Phase -> Type
data Command p = MkCommand
  { -- | The key name for the command, for display purposes.
    getKey :: Maybe Text,
    -- | The shell command to run.
    command :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

makeFieldLabelsNoPrefix ''Command

instance IsString (Command Phase1) where
  fromString = MkCommand Nothing . T.pack

-- | Phase1 commands.
type CommandP1 = Command Phase1

-- | Phase2 commands.
type CommandP2 = Command Phase2

instance AdvancePhase (Command Phase1) where
  type NextPhase (Command Phase1) = (Maybe Text -> Command Phase2)

  advancePhase cmd minit = over' #command f cmd
    where
      f = case minit of
        Nothing -> id
        Just init -> \c -> init <> " && " <> c

-- | Transforms a command into its text to be executed by the shell.
commandToShell :: Command Phase2 -> String
commandToShell = T.unpack . view #command

-- Transforms a command into a 'ProcessConfig'.
--
commandToProcess :: Command Phase1 -> Maybe Text -> ProcessConfig () () ()
commandToProcess command =
  P.shell
    . commandToShell
    . advancePhase command
