-- | Effect for NotifySend.
--
-- @since X.X
module Shrun.Notify.MonadNotifySend
  ( MonadNotifySend (..),
  )
where

import Data.Text qualified as T
import Effects.System.Process qualified as P
import Shrun.Prelude

-- | Effect for notify-send.
--
-- @since X.X
class (Monad m) => MonadNotifySend m where
  -- | Sends a notification via notify-send.
  --
  -- @since X.X
  notify :: Text -> m ()

-- | @since X.X
instance MonadNotifySend IO where
  notify =
    void
      . P.runProcess
      . P.shell
      . T.unpack

-- | @since X.X
instance (MonadNotifySend m) => MonadNotifySend (ReaderT env m) where
  notify = lift . notify
