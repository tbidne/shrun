-- | Effect for DBus.
--
-- @since X.X
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
--
-- @since X.X
class (Monad m) => MonadDBus m where
  -- | Connects to DBus.
  --
  -- @since X.X
  connectSession :: m Client

  -- | Sends a notification to DBus.
  --
  -- @since X.X
  notify :: Client -> Note -> m Notification

-- | @since X.X
instance MonadDBus IO where
  connectSession = DBusC.connectSession
  notify = DBusN.notify

-- | @since X.X
instance (MonadDBus m) => MonadDBus (ReaderT env m) where
  connectSession = lift connectSession
  notify c = lift . notify c
