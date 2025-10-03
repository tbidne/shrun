{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
module Shrun.Command.Types
  ( CommandP (..),
    CommandP1,
    CommandP2,
    commandToProcess,
  )
where

import Data.Hashable (Hashable)
import Data.Text qualified as T
import Effects.System.Process qualified as P
import Shrun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

data CommandPhase
  = CommandPhase1
  | CommandPhase2

-- | Wrapper for shell commands. Eq/Ord is based on the numeric index.
type CommandP :: CommandPhase -> Type
data CommandP p = MkCommandP
  { -- | NonNegative index for the command.
    index :: Natural,
    -- | The key name for the command, for display purposes.
    key :: Maybe Text,
    -- | The shell command to run.
    command :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Hashable)

instance Eq (CommandP p) where
  x == y = x ^. #index == y ^. #index

instance Ord (CommandP p) where
  x <= y = x ^. #index <= y ^. #index

instance
  ( k ~ A_Lens,
    a ~ Natural,
    b ~ Natural
  ) =>
  LabelOptic "index" k (CommandP p) (CommandP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandP a1 a2 a3) ->
        fmap
          (\b -> MkCommandP b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Maybe Text,
    b ~ Maybe Text
  ) =>
  LabelOptic "key" k (CommandP p) (CommandP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandP a1 a2 a3) ->
        fmap
          (\b -> MkCommandP a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Text,
    b ~ Text
  ) =>
  LabelOptic "command" k (CommandP p) (CommandP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandP a1 a2 a3) ->
        fmap
          (\b -> MkCommandP a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

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
commandToProcess :: CommandP1 -> Maybe Text -> CreateProcess
commandToProcess command =
  P.shell
    . commandToShell
    . advancePhase command
