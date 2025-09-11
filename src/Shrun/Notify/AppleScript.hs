-- | Effect for AppleScript.
module Shrun.Notify.AppleScript
  ( notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effects.System.Process qualified as P
import Shrun.Configuration.Data.Notify.System (NotifySystemP (AppleScript))
import Shrun.Notify.MonadNotify
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Prelude

notifyAppleScript ::
  ( HasCallStack,
    MonadProcess m
  ) =>
  ShrunNote ->
  m (Maybe NotifyException)
notifyAppleScript note =
  notify (shrunToAppleScript note) <<&>> \stderr ->
    MkNotifyException note AppleScript stderr
  where
    notify :: (HasCallStack, MonadProcess m) => Text -> m (Maybe Text)
    notify =
      -- TODO: It would be nice to use process's Osstring interface, if it
      -- gets one.
      fmap exitFailureToStderr
        . (`P.readCreateProcessWithExitCode` "")
        . P.shell
        . T.unpack
{-# INLINEABLE notifyAppleScript #-}

shrunToAppleScript :: ShrunNote -> Text
shrunToAppleScript shrunNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          withDoubleQuotes (shrunNote ^. #body % #unNotifyMessage),
          " with title \"Shrun\" ",
          " subtitle ",
          withDoubleQuotes (shrunNote ^. #summary % #unNotifyMessage),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
