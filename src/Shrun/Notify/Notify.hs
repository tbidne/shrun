{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.Notify
  ( NotifyDynamic (..),
    notify,
    ShrunNote (..),
  )
where

import DBus.Notify (UrgencyLevel)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (send)
import Shrun.Notify.Types (NotifyTimeout)
import Shrun.Prelude

-- | Dynamic effect for sending notifications.
data NotifyDynamic :: Effect where
  Notify :: ShrunNote -> NotifyDynamic es ()

type instance DispatchOf NotifyDynamic = Dynamic

notify :: (NotifyDynamic :> es) => ShrunNote -> Eff es ()
notify = send . Notify

-- | Holds notification data.
data ShrunNote = MkShrunNote
  { summary :: Text,
    body :: Text,
    urgency :: UrgencyLevel,
    timeout :: NotifyTimeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ShrunNote
