-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent logging).
module ShellRun.Logging.RegionLogger
  ( -- * Typeclass for logging
    RegionLogger (..),
  )
where

import ShellRun.Logging.Log (Log (..))
import ShellRun.Prelude

-- | `RegionLogger` is a simple typeclass for abstracting logging functions.
type RegionLogger :: (Type -> Type) -> Constraint
class Monad m => RegionLogger m where
  -- | The region type for our logging. For real programs (e.g. 'IO'),
  -- this will likely be 'ConsoleRegion'.
  type Region m

  putLog :: Log -> m ()

  -- | Pushes a log to the region.
  putRegionLog :: Region m -> Log -> m ()

instance RegionLogger m => RegionLogger (ReaderT e m) where
  type Region (ReaderT e m) = Region m
  putLog = lift . putLog
  putRegionLog region = lift . putRegionLog region
