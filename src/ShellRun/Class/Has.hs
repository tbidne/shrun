module ShellRun.Class.Has
  ( HasLegend (..),
    HasTimeout (..),
    HasNativeLog (..),
    HasCommands (..),
  )
where

import Data.Text (Text)
import ShellRun.Types.Env.NativeLog (NativeLog)
import ShellRun.Types.NonNegative (NonNegative)

class HasLegend env where
  getLegend :: env -> Maybe Text

class HasTimeout env where
  getTimeout :: env -> Maybe NonNegative

class HasNativeLog env where
  getNativeLog :: env -> NativeLog

class HasCommands env where
  getCommands :: env -> [Text]