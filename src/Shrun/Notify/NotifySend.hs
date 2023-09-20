-- | Effect for NotifySend.
module Shrun.Notify.NotifySend
  ( -- * Effect
    NotifySendDynamic (..),
    notify,

    -- ** Handler
    runNotifySendDynamicIO,

    -- * Functions
    notifyNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Process.Typed qualified as P
import Shrun.Notify.Notify (ShrunNote)
import Shrun.Notify.Types
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Prelude

-- | Dynamic effect for notify-send.
data NotifySendDynamic :: Effect where
  Notify :: Text -> NotifySendDynamic es ()

type instance DispatchOf NotifySendDynamic = Dynamic

notify :: (NotifySendDynamic :> es) => Text -> Eff es ()
notify = send . Notify

runNotifySendDynamicIO ::
  ( IOE :> es
  ) =>
  Eff (NotifySendDynamic : es) a ->
  Eff es a
runNotifySendDynamicIO = reinterpret P.runTypedProcess $ \_ -> \case
  Notify t ->
    void
      . P.runProcess
      . P.shell
      . T.unpack
      $ t

notifyNotifySend :: (NotifySendDynamic :> es) => ShrunNote -> Eff es ()
notifyNotifySend = notify . shrunToNotifySend

shrunToNotifySend :: ShrunNote -> Text
shrunToNotifySend shrunNote = txt
  where
    txt =
      mconcat
        [ "notify-send ",
          " --app-name Shrun \"",
          shrunNote ^. #summary,
          "\" ",
          (\b -> " \"" <> b <> "\" ") (shrunNote ^. #body),
          ulToNS (shrunNote ^. #urgency),
          timeout
        ]

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeout = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> " --expire-time 0 "
      NotifyTimeoutSeconds s ->
        mconcat
          [ " --expire-time ",
            showt (fromIntegral @_ @Integer s * 1_000)
          ]
