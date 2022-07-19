-- | Provides the 'Timing' class.
--
-- @since 0.1
module Shrun.Effects.Timing
  ( Timing (..),
    withTiming,
    withTiming_,
  )
where

import Data.Time.LocalTime (ZonedTime (..))
import Data.Time.LocalTime qualified as Local
import Data.Time.Relative (RelativeTime)
import Data.Time.Relative qualified as Relative
import Shrun.Prelude
import Shrun.Utils qualified as U
import System.Clock (Clock (Monotonic), TimeSpec)
import System.Clock qualified as C

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => Timing m where
  -- | @since 0.1
  getSystemTime :: m ZonedTime

  -- | Retrieves the current 'TimeSpec', used for easy timing at the
  -- nanosecond level.
  --
  -- @since 0.3.0.1
  getTimeSpec :: m TimeSpec

-- | @since 0.1
instance Timing IO where
  getSystemTime = Local.getZonedTime
  getTimeSpec = C.getTime Monotonic
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}

-- | @since 0.1
instance Timing m => Timing (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getTimeSpec = lift getTimeSpec
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}

-- | Times an action in terms of seconds.
--
-- @since 0.3.0.1
withTiming :: Timing m => m a -> m (RelativeTime, a)
withTiming m = do
  start <- getTimeSpec
  res <- m
  end <- getTimeSpec
  let seconds = U.diffTime start end
  pure (Relative.fromSeconds seconds, res)
{-# INLINEABLE withTiming #-}

-- | 'withTiming' that ignores the result.
--
-- @since 0.3.0.1
withTiming_ :: Timing m => m a -> m RelativeTime
withTiming_ = fmap (view _1) . withTiming
{-# INLINEABLE withTiming_ #-}
