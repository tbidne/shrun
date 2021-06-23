-- | Provides @HasX@ style typeclasses for environment values.
module ShellRun.Class.Has
  ( HasLegend (..),
    HasTimeout (..),
    HasSubLogging (..),
    HasLogQueue (..),
    HasCommands (..),
  )
where

import Data.Text (Text)
import ShellRun.Logging (LogQueue)
import ShellRun.Math (NonNegative)
import ShellRun.Types.Env.SubLogging (SubLogging)

-- | Path to legend file.
class HasLegend env where
  getLegend :: env -> Maybe Text

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe NonNegative

-- | Determines if we should log commands' output.
class HasSubLogging env where
  getSubLogging :: env -> SubLogging

-- | Retrieves the queue that logs are sent to
class HasLogQueue env where
  getLogQueue :: env -> LogQueue

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> [Text]
