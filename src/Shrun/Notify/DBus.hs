-- | Effect for DBus.
module Shrun.Notify.DBus
  ( -- * Effect
    DBusDynamic (..),
    notify,
    connectSession,

    -- ** Handler
    runDBusDynamicIO,

    -- * Functions
    notifyDBus,
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBusC
import DBus.Notify (Hint (Urgency), Note, Notification)
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (interpret, send)
import Shrun.Notify.Notify (ShrunNote)
import Shrun.Notify.Types
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Prelude

-- | Dynamic effect for dbus.
data DBusDynamic :: Effect where
  ConnectSession :: DBusDynamic es Client
  Notify :: Client -> Note -> DBusDynamic es Notification

type instance DispatchOf DBusDynamic = Dynamic

connectSession :: (DBusDynamic :> es) => Eff es Client
connectSession = send ConnectSession

notify :: (DBusDynamic :> es) => Client -> Note -> Eff es Notification
notify c = send . Notify c

runDBusDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (DBusDynamic : es) a ->
  Eff es a
runDBusDynamicIO = interpret $ \_ -> \case
  ConnectSession -> liftIO DBusC.connectSession
  Notify client n -> liftIO $ DBusN.notify client n

notifyDBus :: (DBusDynamic :> es) => Client -> ShrunNote -> Eff es ()
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
