-- | Provides core 'Env' types.
module ShellRun.Data.Env
  ( -- * \"HasX\" style typeclasses required for our concrete Env type.
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasTimeout (..),

    -- * Types
    Timeout,
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
  )
where

import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Prelude

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe Timeout,
    commandLogging :: CommandLogging,
    commandDisplay :: CommandDisplay,
    commands :: [Text]
  }
  deriving (Show)

-- | Type for determining if we stream commands' logs.
data CommandLogging
  = -- | No logging of sub-commands
    Disabled
  | -- | Logging of sub-commands
    Enabled
  deriving stock (Bounded, Eq, Ord, Show)
  deriving (Semigroup, Monoid) via Supremum CommandLogging

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data CommandDisplay
  = -- | Display the command itself, not the key.
    ShowCommand
  | -- | Display the command's key, if it exists, rather
    -- than the key itself.
    ShowKey
  deriving stock (Bounded, Eq, Ord, Show)
  deriving (Semigroup, Monoid) via Supremum CommandDisplay

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe Timeout

-- | Determines if we should log commands' output.
class HasCommandLogging env where
  getCommandLogging :: env -> CommandLogging

-- | Retrieves the queue that logs are sent to
class HasCommandDisplay env where
  getCommandDisplay :: env -> CommandDisplay

instance HasLegend Env where
  getLegend :: Env -> Maybe Text
  getLegend = legend

instance HasTimeout Env where
  getTimeout :: Env -> Maybe Timeout
  getTimeout = timeout

instance HasCommandLogging Env where
  getCommandLogging :: Env -> CommandLogging
  getCommandLogging = commandLogging

instance HasCommandDisplay Env where
  getCommandDisplay :: Env -> CommandDisplay
  getCommandDisplay = commandDisplay

instance HasCommands Env where
  getCommands :: Env -> [Text]
  getCommands = commands
