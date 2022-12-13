-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
--
-- @since 0.1
module Shrun.Logging.RegionLogger
  ( -- * Typeclass for logging
    RegionLogger (..),
  )
where

import Shrun.Logging.Types.Internal (LogMode)
import Shrun.Prelude

-- | `RegionLogger` is a simple typeclass for abstracting logging functions.
--
-- @since 0.1
type RegionLogger :: (Type -> Type) -> Constraint
class Monad m => RegionLogger m where
  -- | The type of the region. This will be ConsoleRegion for production
  -- code and () for tests.
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

  -- | Runs an @m a@ with a console region.
  --
  -- @since 0.5
  withConsoleRegion :: RegionLayout -> (Region m -> m a) -> m a

  -- | Displays the console regions. This should wrap whatever top-level logic
  -- needs regions.
  --
  -- @since 0.7
  displayConsoleRegions :: m a -> m a
