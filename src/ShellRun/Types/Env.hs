{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides core 'Env' types.
module ShellRun.Types.Env
  ( Env (..),
    SubLogging (..),
  )
where

import Data.Text (Text)
import ShellRun.Class.Has
  ( HasCommands (..),
    HasLegend (..),
    HasLogQueue (..),
    HasSubLogging (..),
    HasTimeout (..),
  )
import ShellRun.Logging (LogQueue)
import ShellRun.Math (NonNegative)
import ShellRun.Types.Env.SubLogging (SubLogging (..))

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    nativeLog :: SubLogging,
    logQueue :: LogQueue,
    commands :: [Text]
  }

instance HasLegend Env where
  getLegend = legend

instance HasTimeout Env where
  getTimeout = timeout

instance HasSubLogging Env where
  getSubLogging = nativeLog

instance HasLogQueue Env where
  getLogQueue = logQueue

instance HasCommands Env where
  getCommands = commands
