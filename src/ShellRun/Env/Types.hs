{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
--
-- @since 0.1
module ShellRun.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasLogging (..),
    HasCompletedCmds (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    TruncRegion (..),
    StripControl (..),
  )
where

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
  deriving stock
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
  {-# INLINEABLE compare #-}

-- | @since 0.1
instance Bounded (Truncation a) where
  minBound = MkTruncation PPosInf
  {-# INLINEABLE minBound #-}
  maxBound = MkTruncation (PFin 0)
  {-# INLINEABLE maxBound #-}

makeFieldLabelsNoPrefix ''Truncation

-- | Determines how we should treat control characters encountered in
-- logs.
--
-- @since 0.3
data StripControl
  = -- | Strip all control characters.
    --
    -- @since 0.3
    StripControlAll
  | -- | \"Intelligently\" strip control characters e.g. colors are fine,
    -- ones that affect the cursor should be removed.
    --
    -- @since 0.3
    StripControlSmart
  | -- | Do not strip any control characters.
    --
    -- @since 0.3
    StripControlNone
  deriving stock
    ( -- | @since 0.3
      Bounded,
      -- | @since 0.3
      Enum,
      -- | @since 0.3
      Eq,
      -- | @since 0.3
      Ord,
      -- | @since 0.3
      Show
    )
  deriving
    ( -- | @since 0.3
      Semigroup,
      -- | @since 0.3
      Monoid
    )
    via Supremum StripControl

makePrismLabels ''StripControl

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

-- | Holds logging configuration.
--
-- @since 0.3
class HasLogging env where
  -- | Determines how to display command names.
  --
  -- @since 0.1
  getCmdDisplay :: env -> CmdDisplay

  -- | Determines command line truncation behavior.
  --
  -- @since 0.1
  getCmdLineTrunc :: env -> Truncation 'TCmdLine

  -- | Determines if we should log commands' output to the console.
  --
  -- @since 0.1
  getCmdLogging :: env -> CmdLogging

  -- | Determines command name truncation behavior.
  --
  -- @since 0.1
  getCmdNameTrunc :: env -> Truncation 'TCmdName

  -- | File logging, if any.
  --
  -- @since 0.3
  getFileLogging :: env -> Maybe (FilePath, LogTextQueue)

  -- | Determines if logging is enabled globally.
  --
  -- @since 0.1
  getGlobalLogging :: env -> Bool

  -- | Determines control character behavior.
  --
  -- @since 0.3
  getStripControl :: env -> StripControl

-- | Determines command line truncation behavior.
--
-- @since 0.1
class HasCompletedCmds env where
  -- | @since 0.1
  getCompletedCmds :: env -> TVar (Seq Command)

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Effects.MonadReader'.
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
    -- | Determines to what extent we should remove control characters
    -- from logs.
    --
    -- @since 0.3
    stripControl :: StripControl,
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
  {-# INLINEABLE getLegend #-}

-- | @since 0.1
instance HasTimeout Env where
  getTimeout = view #timeout
  {-# INLINEABLE getTimeout #-}

-- | @since 0.3
instance HasLogging Env where
  getCmdDisplay = view #cmdDisplay
  getCmdLineTrunc = view #lineNameTrunc
  getCmdLogging = view #cmdLogging
  getCmdNameTrunc = view #cmdNameTrunc
  getFileLogging = view #fileLogging
  getGlobalLogging = view #globalLogging
  getStripControl = view #stripControl
  {-# INLINEABLE getCmdDisplay #-}
  {-# INLINEABLE getCmdLineTrunc #-}
  {-# INLINEABLE getCmdLogging #-}
  {-# INLINEABLE getCmdNameTrunc #-}
  {-# INLINEABLE getFileLogging #-}
  {-# INLINEABLE getGlobalLogging #-}
  {-# INLINEABLE getStripControl #-}

-- | @since 0.1
instance HasCompletedCmds Env where
  getCompletedCmds = view #completedCmds
  {-# INLINEABLE getCompletedCmds #-}

-- | @since 0.1
instance HasCommands Env where
  getCommands = view #commands
  {-# INLINEABLE getCommands #-}
