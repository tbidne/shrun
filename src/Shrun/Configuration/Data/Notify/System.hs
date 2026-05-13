{-# LANGUAGE CPP #-}

-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.System
  ( -- * Notify system
    parseNotifySystem,
    notifySystemMeta,
    mergeNotifySystem,
    defNotifySystemStr,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | "Merges" notify systems.
mergeNotifySystem ::
  Maybe NotifySystem ->
  Maybe NotifySystem ->
  NotifySystem
mergeNotifySystem mArgs mToml =
  case mArgs of
    Just s -> s
    Nothing -> case mToml of
      Just t -> t
      Nothing -> defaultNotifySystem

-- | Parses 'NotifySystemOs'.
parseNotifySystem :: (MonadFail m) => m Text -> m NotifySystem
parseNotifySystem = (>>= Utils.inverseMapFail display "notify-system" notifySystemMeta)
{-# INLINEABLE parseNotifySystem #-}

-- | Available 'NotifySystem' strings.
notifySystemMeta :: (IsString a) => Tuple2 Bool (List a)
notifySystemMeta = (False, ["apple-script", "dbus", "notify-send"])

defNotifySystemStr :: Text
defNotifySystemStr = display @NotifySystem def
