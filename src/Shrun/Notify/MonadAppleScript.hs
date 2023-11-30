-- | Effect for AppleScript.
module Shrun.Notify.MonadAppleScript
  ( MonadAppleScript (..),
    notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effects.Process.Typed qualified as P
import Shrun.Notify.MonadNotify (ShrunNote)
import Shrun.Prelude

-- | Effect for apple script.
class (Monad m) => MonadAppleScript m where
  -- | Sends a notification via apple script.
  notify :: Text -> m ()

instance MonadAppleScript IO where
  notify =
    void
      . P.runProcess
      . P.shell
      . T.unpack

instance (MonadAppleScript m) => MonadAppleScript (ReaderT env m) where
  notify = lift . notify

notifyAppleScript :: (MonadAppleScript m) => ShrunNote -> m ()
notifyAppleScript = notify . shrunToAppleScript

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
