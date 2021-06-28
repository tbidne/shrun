{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides core 'Env' types.
module ShellRun.Data.Env
  ( -- * \"HasX\" style typeclasses required for our concrete Env type.
    HasCommandDisplay (..),
    HasLogQueue (..),
    HasCommandLogging (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
  )
where

import Data.Text (Text)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging (LogQueue)
import ShellRun.Math (NonNegative, Supremum (..))

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    commandLogging :: CommandLogging,
    commandDisplay :: CommandDisplay,
    logQueue :: LogQueue,
    commands :: [Text]
  }

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
  getTimeout :: env -> Maybe NonNegative

-- | Determines if we should log commands' output.
class HasCommandLogging env where
  getCommandLogging :: env -> CommandLogging

-- | Retrieves the queue that logs are sent to
class HasCommandDisplay env where
  getCommandDisplay :: env -> CommandDisplay

-- | Retrieves the queue that logs are sent to
class HasLogQueue env where
  getLogQueue :: env -> LogQueue

instance HasLegend Env where
  getLegend = legend

instance HasTimeout Env where
  getTimeout = timeout

instance HasCommandLogging Env where
  getCommandLogging = commandLogging

instance HasCommandDisplay Env where
  getCommandDisplay = commandDisplay

instance HasLogQueue Env where
  getLogQueue = logQueue

instance HasCommands Env where
  getCommands = commands
