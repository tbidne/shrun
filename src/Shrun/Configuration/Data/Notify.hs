{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Shrun.Configuration.Data.Notify
  ( NotifyP (..),
    NotifyArgs,
    NotifyToml,
    NotifyMerged,
    NotifyEnv,
    mergeNotifyLogging,
    toEnv,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
    (<>?.),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Notify.MonadDBus (MonadDBus (connectSession))
import Shrun.Notify.Types
  ( LinuxNotifySystemMismatch (LinuxNotifySystemMismatchAppleScript),
    NotifyAction,
    NotifySystemEnv,
    NotifySystemP (AppleScript, DBus, NotifySend),
    NotifyTimeout,
    OsxNotifySystemMismatch
      ( OsxNotifySystemMismatchDBus,
        OsxNotifySystemMismatchNotifySend
      ),
    displayNotifySystem,
    mergeNotifySystem,
  )
import Shrun.Prelude

-- See NOTE: [Args vs. Toml mandatory fields]

-- | Notify action is mandatory if we are running notifications.
type NotifyActionF :: ConfigPhase -> Type
type family NotifyActionF p where
  NotifyActionF ConfigPhaseArgs = WithDisabled NotifyAction
  NotifyActionF ConfigPhaseToml = NotifyAction
  NotifyActionF ConfigPhaseMerged = NotifyAction
  NotifyActionF ConfigPhaseEnv = NotifyAction

-- | Holds notification config.
type NotifyP :: ConfigPhase -> Type
data NotifyP p = MkNotifyP
  { -- | Actions for which to send notifications.
    action :: NotifyActionF p,
    -- | The notification system to use.
    system :: ConfigPhaseF p (NotifySystemP p),
    -- | when to timeout successful notifications.
    timeout :: ConfigPhaseF p NotifyTimeout
  }

instance
  ( k ~ A_Lens,
    a ~ NotifyActionF p,
    b ~ NotifyActionF p
  ) =>
  LabelOptic "action" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkNotifyP _action _system _timeout) ->
          fmap
            (\action' -> MkNotifyP action' _system _timeout)
            (f _action)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p (NotifySystemP p),
    b ~ ConfigPhaseF p (NotifySystemP p)
  ) =>
  LabelOptic "system" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkNotifyP _action _system _timeout) ->
          fmap
            (\system' -> MkNotifyP _action system' _timeout)
            (f _system)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p NotifyTimeout,
    b ~ ConfigPhaseF p NotifyTimeout
  ) =>
  LabelOptic "timeout" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkNotifyP _action _system _timeout) ->
          fmap
            (MkNotifyP _action _system)
            (f _timeout)
  {-# INLINE labelOptic #-}

type NotifyArgs = NotifyP ConfigPhaseArgs

type NotifyToml = NotifyP ConfigPhaseToml

type NotifyMerged = NotifyP ConfigPhaseMerged

type NotifyEnv = NotifyP ConfigPhaseEnv

deriving stock instance Eq (NotifyP ConfigPhaseArgs)

deriving stock instance Show (NotifyP ConfigPhaseArgs)

deriving stock instance Eq (NotifyP ConfigPhaseToml)

deriving stock instance Show (NotifyP ConfigPhaseToml)

deriving stock instance Eq (NotifyP ConfigPhaseMerged)

deriving stock instance Show (NotifyP ConfigPhaseMerged)

-- | Merges args and toml configs.
mergeNotifyLogging ::
  NotifyArgs ->
  Maybe NotifyToml ->
  Maybe NotifyMerged
mergeNotifyLogging args mToml =
  mAction <&> \action ->
    let toml :: NotifyToml
        toml = fromMaybe (mkDefaultToml action) mToml
     in MkNotifyP
          { action,
            system =
              mergeNotifySystem (args ^. #system) (toml ^. #system),
            timeout =
              (args ^. #timeout) <>?. (toml ^. #timeout)
          }
  where
    mAction = case (args ^. #action, mToml) of
      -- 1. Logging globally disabled
      (Disabled, _) -> Nothing
      -- 2. No Args and no Toml
      (Without, Nothing) -> Nothing
      (With p, _) -> Just p
      (_, Just toml) -> Just $ toml ^. #action

instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyP
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"

#if OSX

toEnv ::
  ( MonadThrow m
  ) =>
  NotifyMerged ->
  m NotifyEnv
toEnv notifyMerged = case systemMerged of
  DBus _ -> throwM OsxNotifySystemMismatchDBus
  NotifySend -> throwM OsxNotifySystemMismatchNotifySend
  AppleScript -> pure $ mkNotify notifyMerged AppleScript
  where
    systemMerged = notifyMerged ^. #system

#else

toEnv ::
  ( MonadDBus m,
    MonadThrow m
  ) =>
  NotifyMerged ->
  m NotifyEnv
toEnv notifyMerged = case systemMerged of
  AppleScript -> throwM LinuxNotifySystemMismatchAppleScript
  DBus _ -> mkNotify notifyMerged . DBus <$> connectSession
  NotifySend -> pure $ mkNotify notifyMerged NotifySend
  where
    systemMerged = notifyMerged ^. #system

#endif

mkNotify :: NotifyMerged -> NotifySystemEnv -> NotifyEnv
mkNotify notifyToml systemP2 =
  MkNotifyP
    { system = systemP2,
      action = notifyToml ^. #action,
      timeout = notifyToml ^. #timeout
    }

mkDefaultToml :: NotifyAction -> NotifyToml
mkDefaultToml action =
  MkNotifyP
    { system = Nothing,
      action = action,
      timeout = Nothing
    }
