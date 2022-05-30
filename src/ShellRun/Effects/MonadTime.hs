-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module ShellRun.Effects.MonadTime
  ( MonadTime (..),
  )
where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import ShellRun.Prelude
import System.Clock (Clock (Monotonic), TimeSpec)
import System.Clock qualified as C

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => MonadTime m where
  -- | @since 0.1
  getSystemTime :: m UTCTime

  -- | Retrieves the current 'TimeSpec', used for easy timing at the
  -- nanosecond level.
  --
  -- @since 0.3.0.1
  getTimeSpec :: m TimeSpec

-- | @since 0.1
instance MonadTime IO where
  getSystemTime = Clock.getCurrentTime
  getTimeSpec = C.getTime Monotonic
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getTimeSpec = lift getTimeSpec
  {-# INLINEABLE getSystemTime #-}
  {-# INLINEABLE getTimeSpec #-}
