{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides core 'Env' types.
module ShellRun.Data.Env
  ( -- * \"HasX\" style typeclasses required for our concrete Env type.
    HasCommandDisplay (..),
    HasLogQueue (..),
    HasSubLogging (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CommandDisplay (..),
    SubLogging (..),
  )
where

import Data.Text (Text)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging (LogQueue)
import ShellRun.Math (NonNegative)

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    subLogging :: SubLogging,
    commandDisplay :: CommandDisplay,
    logQueue :: LogQueue,
    commands :: [Text]
  }

-- | Type for determining if we stream commands' logs.
data SubLogging
  = -- | No logging of sub-commands
    Disabled
  | -- | Logging of sub-commands
    Enabled
  deriving (Show)

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data CommandDisplay
  = -- | Display the command's key, if it exists, rather
    -- than the key itself.
    ShowKey
  | -- | Display the command itself, not the key.
    ShowCommand
  deriving (Show)

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe NonNegative

-- | Determines if we should log commands' output.
class HasSubLogging env where
  getSubLogging :: env -> SubLogging

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

instance HasSubLogging Env where
  getSubLogging = subLogging

instance HasCommandDisplay Env where
  getCommandDisplay = commandDisplay

instance HasLogQueue Env where
  getLogQueue = logQueue

instance HasCommands Env where
  getCommands = commands
