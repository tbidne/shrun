-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module ShellRun.Effects.MonadTime
  ( MonadTime (..),
    withTiming,
    withTiming_,
  )
where

import Data.Time.LocalTime (ZonedTime (..))
import Data.Time.LocalTime qualified as Local
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Clock (Clock (Monotonic), TimeSpec)
import System.Clock qualified as C

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => MonadTime m where
  -- | @since 0.1
  getSystemTime :: m ZonedTime

  -- | Retrieves the current 'TimeSpec', used for easy timing at the
  -- nanosecond level.
  --
  -- @since 0.3.0.1
  getTimeSpec :: m TimeSpec

-- | @since 0.1
instance MonadTime IO where
  getSystemTime = Local.getZonedTime
  getTimeSpec = C.getTime Monotonic
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getTimeSpec = lift getTimeSpec
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}

-- | Times an action.
--
-- @since 0.3.0.1
withTiming :: MonadTime m => m a -> m (a, Natural)
withTiming m = do
  start <- getTimeSpec
  res <- m
  end <- getTimeSpec
  pure (res, U.diffTime start end)
{-# INLINEABLE withTiming #-}

-- | 'withTiming' that ignores the result.
--
-- @since 0.3.0.1
withTiming_ :: MonadTime m => m a -> m Natural
withTiming_ = fmap snd . withTiming
{-# INLINEABLE withTiming_ #-}
