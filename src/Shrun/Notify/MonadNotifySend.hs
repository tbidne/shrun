-- | Effect for NotifySend.
module Shrun.Notify.MonadNotifySend
  ( MonadNotifySend (..),
    notifyNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Data.Text qualified as T
import Effects.Process.Typed qualified as P
import Shrun.Configuration.Data.Notify.System (NotifySystemP (NotifySend))
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.MonadNotify
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Effect for notify-send.
class (Monad m) => MonadNotifySend m where
  -- | Sends a notification via notify-send.
  notify :: (HasCallStack) => Text -> m (Maybe ByteString)

instance MonadNotifySend IO where
  notify =
    fmap exitFailureToStderr
      . P.readProcessStderr
      . P.shell
      . T.unpack
  {-# INLINEABLE notify #-}

instance (MonadNotifySend m) => MonadNotifySend (ReaderT env m) where
  notify = lift . notify
  {-# INLINEABLE notify #-}

notifyNotifySend ::
  ( HasCallStack,
    MonadNotifySend m
  ) =>
  ShrunNote ->
  m (Maybe NotifyException)
notifyNotifySend note =
  notify (shrunToNotifySend note) <<&>> \stderr ->
    MkNotifyException note NotifySend (decodeUtf8Lenient stderr)
{-# INLINEABLE notifyNotifySend #-}

shrunToNotifySend :: ShrunNote -> Text
shrunToNotifySend shrunNote = txt
  where
    txt =
      mconcat
        [ "notify-send ",
          " --app-name Shrun \"",
          summary,
          "\" ",
          (\b -> " \"" <> b <> "\" ") body,
          ulToNS (shrunNote ^. #urgency),
          timeout
        ]

    -- Encountered a bug where notify-send would error when given commands
    -- from the legend that contained quotes and --common-log-key-hide was active.
    -- This is presumably due to the command in the logs like
    --
    --     [Command][some cmd "with quotes"]...
    --
    -- which was then not properly escaped when sent off to notify-send.
    -- Technically the reproducer:
    --
    --     shrun --common-log-key-hide --notify-system notify-send --config=examples/config.toml ui
    --
    -- only required escaping the summary, but we do the same to the body out
    -- of paranoia.
    summary = Utils.escapeDoubleQuotes $ shrunNote ^. #summary % #unUnlinedText
    body = Utils.escapeDoubleQuotes $ shrunNote ^. #body % #unUnlinedText

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeout = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> " --expire-time 0 "
      NotifyTimeoutSeconds s ->
        mconcat
          [ " --expire-time ",
            showt (unsafeConvertIntegral @_ @Integer s * 1_000)
          ]
