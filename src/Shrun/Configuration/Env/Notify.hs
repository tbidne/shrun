{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Transforms NotifyToml into NotifyEnv.
module Shrun.Configuration.Env.Notify
  ( tomlToNotifyEnv,
  )
where

import Effects.Exception (throwString)
import Shrun.Configuration.Env.Types (NotifyEnv (..))
import Shrun.Configuration.Toml (NotifyToml (..))
import Shrun.Data.Phase (AdvancePhase (advancePhase))
import Shrun.Notify.MonadDBus (MonadDBus (connectSession))
import Shrun.Notify.Types
  ( NotifyAction (..),
    NotifySystem (..),
    NotifySystemP1,
    NotifyTimeout (..),
  )
import Shrun.Prelude

-- | Transforms NotifyToml into NotifyEnv. Notifications are off if one of
-- the following is true:
--
-- 1. NotifyToml is completely unspecified (i.e. Nothing)
-- 2. NotifyNone is specified
--
-- On OSX, throws an exception for anything but @tomlToNotifyEnv Nothing@.
tomlToNotifyEnv ::
  ( HasCallStack,
    MonadDBus m,
    MonadThrow m
  ) =>
  Maybe NotifyToml ->
  m (Maybe NotifyEnv)
tomlToNotifyEnv Nothing = pure Nothing
tomlToNotifyEnv (Just (MkNotifyToml NotifyNone _ _)) = pure Nothing
#if OSX
tomlToNotifyEnv _ = throwString "Notifications are only available on linux!"
#else
tomlToNotifyEnv (Just notifyToml) =
  case advancePhase systemP1 of
    Left sys -> pure $ Just $ mkNotify sys
    Right mkDBus -> Just . mkNotify . mkDBus <$> connectSession
  where
    systemP1 = fromMaybe (DBus ()) (notifyToml ^. #system)
    mkNotify systemP2 =
        MkNotifyEnv
          { system = systemP2,
            action = notifyToml ^. #action,
            timeout = fromMaybe (NotifyTimeoutSeconds 10) (notifyToml ^. #timeout)
          }
#endif
