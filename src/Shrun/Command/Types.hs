{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' wrapper for commands.
module Shrun.Command.Types
  ( -- * Command
    CommandP (..),
    CommandP1,
    CommandP2,
    commandToProcess,

    -- ** Index
    CommandIndex,
    Internal.fromPositive,
    Internal.unsafeFromInt,
    Internal.toVertex,
    Internal.fromVertex,
    Internal.succ,
    Internal.addNN,
    Internal.range,
    Internal.joinRange,

    -- * Order
    CommandOrd (..),

    -- * Status
    CommandStatus (..),

    -- ** Map
    CommandStatusMapP (..),
    TCommandStatusMap,
    CommandStatusMap,
    readCommandStatus,

    -- * Misc
    CommandPhase (..),
    Internal.Vertex,
    Internal.LVertex,
  )
where

import Data.Text qualified as T
import Effects.System.Process (Pid)
import Effects.System.Process qualified as P
import Shrun.Command.Types.Internal (CommandIndex)
import Shrun.Command.Types.Internal qualified as Internal
import Shrun.Prelude

-- $setup
-- >>> :set -XOverloadedLists

data CommandPhase
  = CommandPhase1
  | CommandPhase2

-- | Wrapper for shell commands. Whenever the CommandIndex order is important,
-- see 'CommandOrd'.
type CommandP :: CommandPhase -> Type
data CommandP p = MkCommandP
  { -- | NonNegative index for the command.
    index :: CommandIndex,
    -- | The key name for the command, for display purposes.
    key :: Maybe Text,
    -- | The shell command to run.
    command :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

-- NOTE: We use standard Eq here and put the equivalence class Ord on a
-- newtype (CommandOrd) because some of tests verify all CommandP fields
-- match our expectations. Implementing Eq/Ord in terms of index would
-- weaken tests and hide bugs (e.g. potential legend refactors had a bug
-- with the key, but the tests missed this when Eq was determined by index).
--
-- We also intentionally do not implement Ord, as such uses should
-- probably use CommandOrd, and we do not want to accidentally use the
-- derived Ord.

instance
  ( k ~ A_Lens,
    a ~ CommandIndex,
    b ~ CommandIndex
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

-- | Represents a Command's status.
type CommandStatus :: Type
data CommandStatus
  = -- | The command ran successfully.
    CommandSuccess
  | -- | The command failed.
    CommandFailure
  | -- | The command is running. It is associated to its own PID and child
    -- PIDs, for later cleanup.
    CommandRunning (Tuple2 (Maybe Pid) (List Pid))
  | -- | The command is waiting to run.
    CommandWaiting

-- | Wraps 'CommandP' for the purposes of ordering by index.
newtype CommandOrd p = MkCommandOrd (CommandP p)
  deriving newtype (Generic, Hashable)

instance Eq (CommandOrd p) where
  MkCommandOrd x == MkCommandOrd y = x ^. #index == y ^. #index

instance Ord (CommandOrd p) where
  MkCommandOrd x <= MkCommandOrd y = x ^. #index <= y ^. #index

instance
  (k ~ An_Iso, a ~ CommandP p, b ~ CommandP p) =>
  LabelOptic "unCommandOrd" k (CommandOrd p) (CommandOrd p) a b
  where
  labelOptic = iso (\(MkCommandOrd x) -> x) MkCommandOrd
  {-# INLINE labelOptic #-}

-- | Command status map index.
data CommandStatusMapIndex
  = CommandStatusMapStm
  | CommandStatusMapPure

-- | Relates index to command status type.
type CommandStatusMapIndexF :: CommandStatusMapIndex -> Type
type family CommandStatusMapIndexF i where
  CommandStatusMapIndexF CommandStatusMapStm = HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus))
  CommandStatusMapIndexF CommandStatusMapPure = HashMap CommandIndex (Tuple2 CommandP1 CommandStatus)

-- | Command status map.
type CommandStatusMapP :: CommandStatusMapIndex -> Type
newtype CommandStatusMapP p = MkCommandStatusMapP
  { unCommandStatusMap :: CommandStatusMapIndexF p
  }

-- | Normal status map, mutable in stm.
type TCommandStatusMap = CommandStatusMapP CommandStatusMapStm

-- | Pure status map i.e. after the stm map has been read.
type CommandStatusMap = CommandStatusMapP CommandStatusMapPure

instance
  (k ~ An_Iso, a ~ CommandStatusMapIndexF p, b ~ CommandStatusMapIndexF p) =>
  LabelOptic "unCommandStatusMap" k (CommandStatusMapP p) (CommandStatusMapP p) a b
  where
  labelOptic = iso (\(MkCommandStatusMapP x) -> x) MkCommandStatusMapP
  {-# INLINE labelOptic #-}

-- | Reads a map of TVars into a pure map via a single STM transaction.
readCommandStatus ::
  ( HasCallStack,
    MonadAtomic m
  ) =>
  TCommandStatusMap ->
  m CommandStatusMap
readCommandStatus (MkCommandStatusMapP mp) =
  MkCommandStatusMapP <$> atomically (for mp (traverse readTVar'))
