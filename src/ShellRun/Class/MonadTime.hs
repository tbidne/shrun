-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module ShellRun.Class.MonadTime
  ( MonadTime (..),
  )
where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import ShellRun.Prelude

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => MonadTime m where
  -- | @since 0.1
  getSystemTime :: m UTCTime

-- | @since 0.1
instance MonadTime IO where
  getSystemTime = Clock.getCurrentTime

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
