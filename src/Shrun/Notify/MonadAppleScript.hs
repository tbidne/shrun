-- | Effect for AppleScript.
module Shrun.Notify.MonadAppleScript
  ( MonadAppleScript (..),
    notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effects.Process.Typed qualified as P
import Shrun.Notify.MonadNotify
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Notify.Types (NotifySystemP (AppleScript))
import Shrun.Prelude

-- | Effect for apple script.
class (Monad m) => MonadAppleScript m where
  -- | Sends a notification via apple script.
  notify :: (HasCallStack) => Text -> m (Maybe ByteString)

instance MonadAppleScript IO where
  notify =
    fmap exitFailureToStderr
      . P.readProcessStderr
      . P.shell
      . T.unpack

instance (MonadAppleScript m) => MonadAppleScript (ReaderT env m) where
  notify = lift . notify

notifyAppleScript ::
  ( HasCallStack,
    MonadAppleScript m
  ) =>
  ShrunNote ->
  m (Maybe NotifyException)
notifyAppleScript note =
  notify (shrunToAppleScript note) <<&>> \stderr ->
    MkNotifyException note AppleScript (decodeUtf8Lenient stderr)

shrunToAppleScript :: ShrunNote -> Text
shrunToAppleScript shrunNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          withDoubleQuotes (shrunNote ^. #body),
          " with title \"Shrun\" ",
          " subtitle ",
          withDoubleQuotes (shrunNote ^. #summary),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
