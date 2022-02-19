-- | Provides types and typeclasses for our environment.
--
-- @since 0.1.0.0
module ShellRun.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasCmdTruncation (..),
    HasLineTruncation (..),
    HasFileLogging (..),
    HasLegend (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
    Truncation (..),
    TruncationArea (..),
  )
where

import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Logging.Queue (LogTextQueue)
import ShellRun.Prelude

-- | Path to legend file.
--
-- @since 0.1.0.0
class HasLegend env where
  -- | @since 0.1.0.0
  getLegend :: env -> Maybe FilePath

-- | The commands themselves.
--
-- @since 0.1.0.0
class HasCommands env where
  -- | @since 0.1.0.0
  getCommands :: env -> NonEmptySeq Text

-- | Timeout, if any.
--
-- @since 0.1.0.0
class HasTimeout env where
  -- | @since 0.1.0.0
  getTimeout :: env -> Timeout

-- | FileLogging, if any.
--
-- @since 0.1.0.0
class HasFileLogging env where
  -- | @since 0.1.0.0
  getFileLogging :: env -> Maybe (FilePath, LogTextQueue)

-- | Determines if we should log commands' output.
--
-- @since 0.1.0.0
class HasCommandLogging env where
  getCommandLogging :: env -> CommandLogging

-- | Determines how to display command names.
--
-- @since 0.1.0.0
class HasCommandDisplay env where
  -- | @since 0.1.0.0
  getCommandDisplay :: env -> CommandDisplay

-- | Determines command truncation behavior.
--
-- @since 0.1.0.0
class HasCmdTruncation env where
  -- | @since 0.1.0.0
  getCmdTruncation :: env -> Truncation 'TCommand

-- | Determines line truncation behavior.
--
-- @since 0.1.0.0
class HasLineTruncation env where
  -- | @since 0.1.0.0
  getLineTruncation :: env -> Truncation 'TLine

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
--
-- @since 0.1.0.0
data Env = MkEnv
  { -- | Optional path to a legend file.
    --
    -- @since 0.1.0.0
    legend :: Maybe FilePath,
    -- | Timeout.
    --
    -- @since 0.1.0.0
    timeout :: Timeout,
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.1.0.0
    fileLogging :: Maybe (Tuple2 FilePath LogTextQueue),
    -- | Whether to log commands.
    --
    -- @since 0.1.0.0
    commandLogging :: CommandLogging,
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.1.0.0
    commandDisplay :: CommandDisplay,
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1.0.0
    cmdTruncation :: Truncation 'TCommand,
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1.0.0
    lineTruncation :: Truncation 'TLine,
    -- | The commands to run.
    --
    -- @since 0.1.0.0
    commands :: NonEmptySeq Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance HasLegend Env where
  getLegend = legend

-- | @since 0.1.0.0
instance HasTimeout Env where
  getTimeout = timeout

-- | @since 0.1.0.0
instance HasFileLogging Env where
  getFileLogging = fileLogging

-- | @since 0.1.0.0
instance HasCommandLogging Env where
  getCommandLogging = commandLogging

-- | @since 0.1.0.0
instance HasCommandDisplay Env where
  getCommandDisplay = commandDisplay

-- | @since 0.1.0.0
instance HasCmdTruncation Env where
  getCmdTruncation = cmdTruncation

-- | @since 0.1.0.0
instance HasLineTruncation Env where
  getLineTruncation = lineTruncation

-- | @since 0.1.0.0
instance HasCommands Env where
  getCommands = commands

-- | Type for determining if we stream commands' logs.
--
-- @since 0.1.0.0
data CommandLogging
  = -- | No logging of sub-commands.
    --
    -- @since 0.1.0.0
    Disabled
  | -- | Logging of sub-commands
    --
    -- @since 0.1.0.0
    Enabled
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum CommandLogging

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data CommandDisplay
  = -- | Display the command itself, not the key.
    -- @since 0.1.0.0
    ShowCommand
  | -- | Display the command's key, if it exists, rather
    -- than the key itself.
    --
    -- @since 0.1.0.0
    ShowKey
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum CommandDisplay

data TruncationArea
  = TCommand
  | TLine

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
-- >>> mempty @(Truncation TCommand)
-- MkTruncation {unTruncation = PPosInf}
--
-- @since 0.1.0.0
type Truncation :: TruncationArea -> Type
newtype Truncation a = MkTruncation
  { -- | @since 0.1.0.0
    unTruncation :: PosInfNum Natural
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum (Truncation a)

-- | @since 0.1.0.0
instance Ord (Truncation a) where
  compare x y | x == y = EQ
  compare (MkTruncation PPosInf) _ = LT
  compare _ (MkTruncation PPosInf) = GT
  compare (MkTruncation (PFin x)) (MkTruncation (PFin y)) = compare y x

-- | @since 0.1.0.0
instance Bounded (Truncation a) where
  minBound = MkTruncation PPosInf
  maxBound = MkTruncation (PFin 0)
