-- | Effect for DBus.
module Shrun.Notify.MonadDBus
  ( MonadDBus (..),
    notifyDBus,
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBusC
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Shrun.Configuration.Data.Notify.System (NotifySystemP (DBus))
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.MonadNotify (NotifyException (MkNotifyException), ShrunNote)
import Shrun.Prelude

-- | Effect for DBus.
class (Monad m) => MonadDBus m where
  -- | Connects to DBus.
  connectSession :: (HasCallStack) => m Client

  -- | Sends a notification to DBus.
  notify :: (HasCallStack) => Client -> Note -> m (Maybe SomeException)

instance MonadDBus IO where
  connectSession = DBusC.connectSession
  {-# INLINEABLE connectSession #-}

  notify client note =
    tryAny (DBusN.notify client note) <&> \case
      Left err -> Just err
      Right _ -> Nothing
  {-# INLINEABLE notify #-}

instance (MonadDBus m) => MonadDBus (ReaderT env m) where
  connectSession = lift connectSession
  {-# INLINEABLE connectSession #-}
  notify c = lift . notify c
  {-# INLINEABLE notify #-}

notifyDBus ::
  ( HasCallStack,
    MonadDBus m
  ) =>
  Client ->
  ShrunNote ->
  m (Maybe NotifyException)
notifyDBus client note =
  notify client (shrunToDBus note) <<&>> \stderr ->
    MkNotifyException note (DBus ()) (T.pack $ displayException stderr)
{-# INLINEABLE notifyDBus #-}

shrunToDBus :: ShrunNote -> Note
shrunToDBus shrunNote =
  DBusN.Note
    { appName = "Shrun",
      summary = unpack $ shrunNote ^. #summary % #unUnlinedText,
      body = Just . DBusN.Text . T.unpack $ shrunNote ^. #body % #unUnlinedText,
      appImage = Nothing,
      hints = [Urgency (shrunNote ^. #urgency)],
      expiry,
      actions = []
    }
  where
    expiry = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> DBusN.Never
      NotifyTimeoutSeconds s ->
        DBusN.Milliseconds $ 1_000 * unsafeConvertIntegral s
