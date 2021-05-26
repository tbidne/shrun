module ShellRun.Types.Env
  ( Env (..),
    NativeLog (..),
    defaultEnv,
  )
where

import Data.Text (Text)
import ShellRun.Types.NonNegative (NonNegative)

data NativeLog
  = None
  | Stdout
  deriving (Show)

data Env = MkEnv
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    nativeLog :: NativeLog,
    commands :: [Text]
  }
  deriving (Show)

defaultEnv :: Env
defaultEnv =
  MkEnv
    { legend = Nothing,
      timeout = Nothing,
      nativeLog = None,
      commands = []
    }