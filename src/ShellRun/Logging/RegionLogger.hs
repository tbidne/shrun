-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
--
-- @since 0.1.0.0
module ShellRun.Logging.RegionLogger
  ( -- * Typeclass for logging
    RegionLogger (..),
  )
where

import ShellRun.Logging.Log (Log (..))
import ShellRun.Prelude

-- | `RegionLogger` is a simple typeclass for abstracting logging functions.
--
-- @since 0.1.0.0
type RegionLogger :: (Type -> Type) -> Constraint
class Monad m => RegionLogger m where
  -- | The region type for our logging. For real programs (e.g. 'IO'),
  -- this will likely be 'System.Console.Regions.ConsoleRegion'.
  --
  -- @since 0.1.0.0
  type Region m

  -- | Pushes a log to the "global" region.
  --
  -- @since 0.1.0.0
  putLog :: Log -> m ()

  -- | Pushes a log to the region.
  --
  -- @since 0.1.0.0
  putRegionLog :: Region m -> Log -> m ()

-- | @since 0.1.0.0
instance RegionLogger m => RegionLogger (ReaderT e m) where
  type Region (ReaderT e m) = Region m
  putLog = lift . putLog
  putRegionLog region = lift . putRegionLog region
