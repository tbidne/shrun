-- | Provides @HasX@ style typeclasses for environment values.
module ShellRun.Class.Has
  ( HasLegend (..),
    HasTimeout (..),
    HasNativeLog (..),
    HasCommands (..),
  )
where

import Data.Text (Text)
import ShellRun.Math (NonNegative)
import ShellRun.Types.Env.NativeLog (NativeLog)

-- | Path to legend file.
class HasLegend env where
  getLegend :: env -> Maybe Text

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe NonNegative

-- | Determines if we should log commands' output.
class HasNativeLog env where
  getNativeLog :: env -> NativeLog

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> [Text]
