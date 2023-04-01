-- | Effect for DBus.
module Shrun.Notify.MonadDBus
  ( MonadDBus (..),
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBusC
import DBus.Notify (Note, Notification)
import DBus.Notify qualified as DBusN
import Shrun.Prelude

-- | Effect for DBus.
class (Monad m) => MonadDBus m where
  -- | Connects to DBus.
  connectSession :: m Client

  -- | Sends a notification to DBus.
  notify :: Client -> Note -> m Notification

instance MonadDBus IO where
  connectSession = DBusC.connectSession
  notify = DBusN.notify

instance (MonadDBus m) => MonadDBus (ReaderT env m) where
  connectSession = lift connectSession
  notify c = lift . notify c
