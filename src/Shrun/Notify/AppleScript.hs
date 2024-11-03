-- | Effect for AppleScript.
module Shrun.Notify.AppleScript
  ( notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effects.Process.Typed qualified as P
import Shrun.Configuration.Data.Notify.System (NotifySystemP (AppleScript))
import Shrun.Notify.MonadNotify
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Prelude

notifyAppleScript ::
  ( HasCallStack,
    MonadTypedProcess m
  ) =>
  ShrunNote ->
  m (Maybe NotifyException)
notifyAppleScript note =
  notify (shrunToAppleScript note) <<&>> \stderr ->
    MkNotifyException note AppleScript (decodeUtf8Lenient stderr)
  where
    notify :: (HasCallStack, MonadTypedProcess m) => Text -> m (Maybe ByteString)
    notify =
      fmap exitFailureToStderr
        . P.readProcessStderr
        . P.shell
        . T.unpack
{-# INLINEABLE notifyAppleScript #-}

shrunToAppleScript :: ShrunNote -> Text
shrunToAppleScript shrunNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          withDoubleQuotes (shrunNote ^. #body % #unUnlinedText),
          " with title \"Shrun\" ",
          " subtitle ",
          withDoubleQuotes (shrunNote ^. #summary % #unUnlinedText),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
