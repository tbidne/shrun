module ShellRun.Types.Args
  ( Args (..),
    NativeLog (..),
  )
where

import Data.Text (Text)
import ShellRun.Types.NonNegative (NonNegative)

data NativeLog
  = None
  | Stdout
  deriving (Show)

data Args = MkArgs
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    nativeLog :: NativeLog,
    commands :: [Text]
  }
  deriving (Show)