-- | Module defining \"HasX\" style typeclasses for the environment
-- needed by ShellRun.
--
-- @since 0.1.0.0
module ShellRun.Env
  ( HasCommands (..),
    HasLegend (..),
  )
where

import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Prelude

-- | Path to legend file.
--
-- @since 0.1.0.0
class HasLegend env where
  -- | @since 0.1.0.0
  getLegend :: env -> Maybe FilePath

-- | The commands themselves.
--
-- @since 0.1.0.0
class HasCommands env where
  -- | @since 0.1.0.0
  getCommands :: env -> NonEmptySeq Text
