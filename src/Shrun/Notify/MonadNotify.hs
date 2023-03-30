{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
--
-- @since X.X
module Shrun.Notify.MonadNotify
  ( MonadNotify (..),
    ShrunNote (..),
  )
where

import DBus.Notify (UrgencyLevel)
import Shrun.Notify.Types (NotifyTimeout)
import Shrun.Prelude

-- | Holds notification data.
--
-- @since X.X
data ShrunNote = MkShrunNote
  { summary :: !Text,
    -- | @since X.X
    body :: !Text,
    -- | @since X.X
    urgency :: !UrgencyLevel,
    -- | @since X.X
    timeout :: !NotifyTimeout
  }
  deriving stock
    ( -- | @since X.X
      Eq,
      -- | @since X.X
      Show
    )

-- | @since X.X
makeFieldLabelsNoPrefix ''ShrunNote

-- | General effect for sending notifications.
--
-- @since X.X
class (Monad m) => MonadNotify m where
  notify :: ShrunNote -> m ()
