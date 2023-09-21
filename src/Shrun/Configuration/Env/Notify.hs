{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Transforms NotifyToml into NotifyEnv.
module Shrun.Configuration.Env.Notify
  ( tomlToNotifyEnv,
  )
where

import Effects.Exception (throwString)
import Shrun.Configuration.Env.Types
  ( NotifyEnv
      ( MkNotifyEnv,
        action,
        system,
        timeout
      ),
  )
import Shrun.Configuration.Toml (NotifyToml)
import Shrun.Data.Phase (AdvancePhase (advancePhase))
import Shrun.Notify.MonadDBus (MonadDBus (connectSession))
import Shrun.Notify.Types
  ( LinuxNotifySystemMismatch (LinuxNotifySystemMismatchAppleScript),
    NotifyAction,
    NotifySystem (AppleScript, DBus),
    NotifySystemP1,
    NotifySystemP2,
    NotifyTimeout (NotifyTimeoutSeconds),
    OsxNotifySystemMismatch
      ( OsxNotifySystemMismatchDBus,
        OsxNotifySystemMismatchNotifySend
      ),
    _AppleScript,
    _DBus,
    _NotifySend,
  )
import Shrun.Prelude

-- | Transforms NotifyToml into NotifyEnv. Notifications are off if
-- NotifyToml is completely unspecified (i.e. Nothing).
tomlToNotifyEnv ::
  ( HasCallStack,
    MonadDBus m,
    MonadThrow m
  ) =>
  Maybe NotifyToml ->
  m (Maybe NotifyEnv)
tomlToNotifyEnv Nothing = pure Nothing
tomlToNotifyEnv (Just notifyToml) = tomlToNotifyEnvOS notifyToml

#if OSX

tomlToNotifyEnvOS ::
  ( HasCallStack,
    MonadDBus m,
    MonadThrow m
  ) =>
  NotifyToml ->
  m (Maybe NotifyEnv)
tomlToNotifyEnvOS (notifyToml)
  | is (#system %? _DBus) notifyToml = throwM OsxNotifySystemMismatchDBus
  | is (#system %? _NotifySend) notifyToml = throwM OsxNotifySystemMismatchNotifySend
  | otherwise = case advancePhase systemP1 of
    Left sys -> pure $ Just $ mkNotify notifyToml sys
    Right mkDBus -> Just . mkNotify notifyToml . mkDBus <$> connectSession
    where
      systemP1 = fromMaybe AppleScript (notifyToml ^. #system)

#else

tomlToNotifyEnvOS ::
  ( HasCallStack,
    MonadDBus m,
    MonadThrow m
  ) =>
  NotifyToml ->
  m (Maybe NotifyEnv)
tomlToNotifyEnvOS notifyToml
  | is (#system %? _AppleScript) notifyToml = throwM LinuxNotifySystemMismatchAppleScript
  | otherwise = case advancePhase systemP1 of
    Left sys -> pure $ Just $ mkNotify notifyToml sys
    Right mkDBus -> Just . mkNotify notifyToml . mkDBus <$> connectSession
    where
      systemP1 = fromMaybe (DBus ()) (notifyToml ^. #system)

#endif

mkNotify :: NotifyToml -> NotifySystemP2 -> NotifyEnv
mkNotify notifyToml systemP2 =
  MkNotifyEnv
    { system = systemP2,
      action = notifyToml ^. #action,
      timeout = fromMaybe (NotifyTimeoutSeconds 10) (notifyToml ^. #timeout)
    }
