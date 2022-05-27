-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
--
-- @since 0.1
module ShellRun.Logging.RegionLogger
  ( -- * Typeclass for logging
    RegionLogger (..),
  )
where

import ShellRun.Logging.Types (LogMode)
import ShellRun.Prelude

-- | `RegionLogger` is a simple typeclass for abstracting logging functions.
--
-- @since 0.1
type RegionLogger :: (Type -> Type) -> Constraint
class Monad m => RegionLogger m where
  -- | The region type for our logging. For real programs (e.g. 'IO'),
  -- this will likely be 'System.Console.Regions.ConsoleRegion'.
  --
  -- @since 0.1
  type Region m

  -- | Pushes a log to the "global" region.
  --
  -- @since 0.1
  logFn :: Text -> m ()

  -- | Selects a region fn based on the 'LogMode'.
  --
  -- @since 0.3
  logModeToRegionFn :: LogMode -> Region m -> Text -> m ()
