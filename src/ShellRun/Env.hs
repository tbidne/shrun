-- | Module defining \"HasX\" style typeclasses for the environment
-- needed by ShellRun.
module ShellRun.Env
  ( HasCommands (..),
    HasLegend (..),
  )
where

import Data.Text (Text)

-- | Path to legend file.
class HasLegend env where
  getLegend :: env -> Maybe Text

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> [Text]