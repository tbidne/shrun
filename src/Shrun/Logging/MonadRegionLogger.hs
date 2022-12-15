-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
--
-- @since 0.1
module Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger (..),
  )
where

import Shrun.Logging.Types.Internal (LogMode)
import Shrun.Prelude

-- | `MonadRegionLogger` is a simple typeclass for abstracting logging functions.
--
-- @since 0.1
type MonadRegionLogger :: (Type -> Type) -> Constraint
class Monad m => MonadRegionLogger m where
  -- | The type of the region. This will be ConsoleRegion for production
  -- code and () for tests.
  --
  -- @since 0.1
  type Region m

  -- | Pushes a log to the "global" region.
  --
  -- @since 0.7
  logGlobal :: Text -> m ()

  -- | Pushes a log to the region.
  --
  -- @since 0.7
  logRegion :: LogMode -> Region m -> Text -> m ()

  -- | Runs an @m a@ with a region.
  --
  -- @since 0.7
  withRegion :: RegionLayout -> (Region m -> m a) -> m a

  -- | Displays the regions. This should wrap whatever top-level logic
  -- needs regions.
  --
  -- @since 0.7
  displayRegions :: m a -> m a