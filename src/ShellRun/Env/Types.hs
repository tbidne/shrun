{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
--
-- @since 0.1
module ShellRun.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCmdDisplay (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasCmdLineTrunc (..),
    HasCompletedCmds (..),
    HasFileLogging (..),
    HasGlobalLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    TruncRegion (..),
  )
where

import Control.Concurrent.STM.TVar (TVar)
import Data.Sequence (Seq)
import ShellRun.Command (Command)
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Logging.Queue (LogTextQueue)
import ShellRun.Prelude

-- | Type for determining if we stream commands' logs.
--
-- @since 0.1
data CmdLogging
  = -- | No logging of sub-commands.
    --
    -- @since 0.1
    Disabled
  | -- | Logging of sub-commands
    --
    -- @since 0.1
    Enabled
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum CmdLogging

makePrismLabels ''CmdLogging

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data CmdDisplay
  = -- | Display the command itself, not the key.
    --
    -- @since 0.1
    ShowCmd
  | -- | Display the command's key, if it exists, rather
    -- than the key itself.
    --
    -- @since 0.1
    ShowKey
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum CmdDisplay

makePrismLabels ''CmdDisplay

-- | The different regions to apply truncation rules.
--
-- @since 0.1
data TruncRegion
  = -- | Apply truncation rules to commands/key names.
    --
    -- @since 0.1
    TCmdName
  | -- | Apply truncation rules to command log entire lines.
    --
    -- @since 0.1
    TCmdLine
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrismLabels ''TruncRegion

-- | The maximum number of command characters to display in the logs.
-- The ordering is such that smaller numbers are greater, i.e., the
-- minimum element is 'PPosInf' and the maximum is zero.
--
-- ==== __Examples__
-- >>> max (MkTruncation PPosInf) (MkTruncation (PFin 7))
-- MkTruncation {unTruncation = PFin 7}
--
-- >>> MkTruncation (PFin 7) <> MkTruncation (PFin 10)
-- MkTruncation {unTruncation = PFin 7}
--
-- >>> mempty @(Truncation TCmdName)
-- MkTruncation {unTruncation = PPosInf}
--
-- @since 0.1
type Truncation :: TruncRegion -> Type
newtype Truncation a = MkTruncation
  { -- | @since 0.1
    unTruncation :: PosInfNum Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum (Truncation a)

-- | @since 0.1
instance Ord (Truncation a) where
  compare x y | x == y = EQ
  compare (MkTruncation PPosInf) _ = LT
  compare _ (MkTruncation PPosInf) = GT
  compare (MkTruncation (PFin x)) (MkTruncation (PFin y)) = compare y x

-- | @since 0.1
instance Bounded (Truncation a) where
  minBound = MkTruncation PPosInf
  maxBound = MkTruncation (PFin 0)

makeFieldLabelsNoPrefix ''Truncation

-- | Path to legend file.
--
-- @since 0.1
class HasLegend env where
  -- | @since 0.1
  getLegend :: env -> FilePathDefault

-- | The commands themselves.
--
-- @since 0.1
class HasCommands env where
  -- | @since 0.1
  getCommands :: env -> NonEmptySeq Text

-- | Timeout, if any.
--
-- @since 0.1
class HasTimeout env where
  -- | @since 0.1
  getTimeout :: env -> Timeout

-- | FileLogging, if any.
--
-- @since 0.1
class HasFileLogging env where
  -- | @since 0.1
  getFileLogging :: env -> Maybe (FilePath, LogTextQueue)

-- | Determines if we should log commands' output to the console.
--
-- @since 0.1
class HasCmdLogging env where
  getCmdLogging :: env -> CmdLogging

-- | Determines how to display command names.
--
-- @since 0.1
class HasCmdDisplay env where
  -- | @since 0.1
  getCmdDisplay :: env -> CmdDisplay

-- | Determines command name truncation behavior.
--
-- @since 0.1
class HasCmdNameTrunc env where
  -- | @since 0.1
  getCmdNameTrunc :: env -> Truncation 'TCmdName

-- | Determines command line truncation behavior.
--
-- @since 0.1
class HasCmdLineTrunc env where
  -- | @since 0.1
  getCmdLineTrunc :: env -> Truncation 'TCmdLine

-- | Determines command line truncation behavior.
--
-- @since 0.1
class HasCompletedCmds env where
  -- | @since 0.1
  getCompletedCmds :: env -> TVar (Seq Command)

-- | Determines command line truncation behavior.
--
-- @since 0.1
class HasGlobalLogging env where
  -- | @since 0.1
  getGlobalLogging :: env -> Bool

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
--
-- @since 0.1
data Env = MkEnv
  { -- | Optional path to a legend file.
    --
    -- @since 0.1
    legend :: FilePathDefault,
    -- | Timeout.
    --
    -- @since 0.1
    timeout :: Timeout,
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.1
    fileLogging :: Maybe (Tuple2 FilePath LogTextQueue),
    -- | Whether to log commands.
    --
    -- @since 0.1
    cmdLogging :: CmdLogging,
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.1
    cmdDisplay :: CmdDisplay,
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1
    cmdNameTrunc :: Truncation 'TCmdName,
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1
    lineNameTrunc :: Truncation 'TCmdLine,
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    --
    -- @since 0.1
    completedCmds :: TVar (Seq Command),
    -- | Overarching option for logging. If it is false then all logging is
    -- disabled.
    --
    -- @since 0.1
    globalLogging :: Bool,
    -- | The commands to run.
    --
    -- @since 0.1
    commands :: NonEmptySeq Text
  }

makeFieldLabelsNoPrefix ''Env

-- | @since 0.1
instance HasLegend Env where
  getLegend = view #legend

-- | @since 0.1
instance HasTimeout Env where
  getTimeout = view #timeout

-- | @since 0.1
instance HasFileLogging Env where
  getFileLogging = view #fileLogging

-- | @since 0.1
instance HasCmdLogging Env where
  getCmdLogging = view #cmdLogging

-- | @since 0.1
instance HasCmdDisplay Env where
  getCmdDisplay = view #cmdDisplay

-- | @since 0.1
instance HasCmdNameTrunc Env where
  getCmdNameTrunc = view #cmdNameTrunc

-- | @since 0.1
instance HasCmdLineTrunc Env where
  getCmdLineTrunc = view #lineNameTrunc

-- | @since 0.1
instance HasCompletedCmds Env where
  getCompletedCmds = view #completedCmds

-- | @since 0.1
instance HasCommands Env where
  getCommands = view #commands

instance HasGlobalLogging Env where
  getGlobalLogging = view #globalLogging
