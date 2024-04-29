{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.MonadNotify
  ( MonadNotify (..),
    ShrunNote (..),
    NotifyException (..),
    exitFailureToStderr,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Shrun.Notify.Types
  ( NotifySystemMerged,
    NotifyTimeout,
    displayNotifySystem,
  )
import Shrun.Prelude

-- | Holds notification data.
data ShrunNote = MkShrunNote
  { summary :: Text,
    body :: Text,
    urgency :: UrgencyLevel,
    timeout :: NotifyTimeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ShrunNote

-- | Exception for sending desktop notifications.
data NotifyException = MkNotifyException ShrunNote NotifySystemMerged Text
  deriving stock (Eq, Show)

instance Exception NotifyException where
  displayException (MkNotifyException note system message) =
    mconcat
      [ "Exception sending notification with system '",
        displayNotifySystem system,
        "' and note '",
        show note,
        "': ",
        T.unpack message
      ]

-- | General effect for sending notifications.
class (Monad m) => MonadNotify m where
  notify :: ShrunNote -> m (Maybe NotifyException)

-- | Maps (ExitCode, stderr) to Just stderr, if the exit code is
-- ExitFailure.
exitFailureToStderr :: (ExitCode, BSL.ByteString) -> Maybe ByteString
exitFailureToStderr (ex, stderr) = case ex of
  ExitSuccess -> Nothing
  ExitFailure _ -> Just (BSL.toStrict stderr)
