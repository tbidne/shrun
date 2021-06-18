-- | Provides core 'Env' types.
module ShellRun.Types.Env
  ( Env (..),
    SubLogging (..),
    defaultEnv,
  )
where

import Data.Text (Text)
import ShellRun.Class.Has
  ( HasCommands (..),
    HasLegend (..),
    HasSubLogging (..),
    HasTimeout (..),
  )
import ShellRun.Math (NonNegative)
import ShellRun.Types.Env.SubLogging (SubLogging (..))

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    nativeLog :: SubLogging,
    commands :: [Text]
  }
  deriving (Show)

instance HasLegend Env where
  getLegend = legend

instance HasTimeout Env where
  getTimeout = timeout

instance HasSubLogging Env where
  getSubLogging = nativeLog

instance HasCommands Env where
  getCommands = commands

-- | Constructs a default 'Env'.
defaultEnv :: Env
defaultEnv =
  MkEnv
    { legend = Nothing,
      timeout = Nothing,
      nativeLog = None,
      commands = []
    }
