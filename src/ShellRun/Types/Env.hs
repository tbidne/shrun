-- | Provides core 'Env' types.
module ShellRun.Types.Env
  ( Env (..),
    NativeLog (..),
    defaultEnv,
  )
where

import Data.Text (Text)
import ShellRun.Class.Has
  ( HasCommands (..),
    HasLegend (..),
    HasNativeLog (..),
    HasTimeout (..),
  )
import ShellRun.Math (NonNegative)
import ShellRun.Types.Env.NativeLog (NativeLog (..))

-- | The main 'Env' type used by Shell Run. Intended to be used with
-- 'MonadReader'.
data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    nativeLog :: NativeLog,
    commands :: [Text]
  }
  deriving (Show)

instance HasLegend Env where
  getLegend = legend

instance HasTimeout Env where
  getTimeout = timeout

instance HasNativeLog Env where
  getNativeLog = nativeLog

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
