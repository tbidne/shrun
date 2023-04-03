-- | Effect for NotifySend.
module Shrun.Notify.MonadNotifySend
  ( MonadNotifySend (..),
    notifyNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Effects.System.Process qualified as P
import Shrun.Notify.MonadNotify (ShrunNote)
import Shrun.Notify.Types (NotifyTimeout (..))
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

notifyNotifySend :: (MonadNotifySend m) => ShrunNote -> m ()
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
