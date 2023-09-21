{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.MonadNotify
  ( MonadNotify (..),
    ShrunNote (..),
  )
where

import DBus.Notify (UrgencyLevel)
import Shrun.Notify.Types (NotifyTimeout)
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

-- | General effect for sending notifications.
class (Monad m) => MonadNotify m where
  notify :: ShrunNote -> m ()
