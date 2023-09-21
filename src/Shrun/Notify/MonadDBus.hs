-- | Effect for DBus.
module Shrun.Notify.MonadDBus
  ( MonadDBus (..),
    notifyDBus,
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBusC
import DBus.Notify (Hint (Urgency), Note, Notification)
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Shrun.Notify.MonadNotify (ShrunNote)
import Shrun.Notify.Types
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
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

notifyDBus :: (MonadDBus m) => Client -> ShrunNote -> m ()
notifyDBus client = void . notify client . shrunToDBus

shrunToDBus :: ShrunNote -> Note
shrunToDBus shrunNote =
  DBusN.Note
    { appName = "Shrun",
      summary = unpack $ shrunNote ^. #summary,
      body = Just . DBusN.Text . T.unpack $ shrunNote ^. #body,
      appImage = Nothing,
      hints = [Urgency (shrunNote ^. #urgency)],
      expiry,
      actions = []
    }
  where
    expiry = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> DBusN.Never
      NotifyTimeoutSeconds s ->
        DBusN.Milliseconds $ 1_000 * fromIntegral s
