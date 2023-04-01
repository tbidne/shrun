-- | Effect for NotifySend.
module Shrun.Notify.MonadNotifySend
  ( MonadNotifySend (..),
  )
where

import Data.Text qualified as T
import Effects.System.Process qualified as P
import Shrun.Prelude

-- | Effect for notify-send.
class (Monad m) => MonadNotifySend m where
  -- | Sends a notification via notify-send.
  notify :: Text -> m ()

instance MonadNotifySend IO where
  notify =
    void
      . P.runProcess
      . P.shell
      . T.unpack

instance (MonadNotifySend m) => MonadNotifySend (ReaderT env m) where
  notify = lift . notify
