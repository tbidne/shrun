{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
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
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Notify.MonadDBus (MonadDBus (connectSession))
import Shrun.Notify.Types
  ( LinuxNotifySystemMismatch (LinuxNotifySystemMismatchAppleScript),
    NotifyAction,
    NotifySystemEnv,
    NotifySystemP (..),
    NotifyTimeout,
    OsxNotifySystemMismatch
      ( OsxNotifySystemMismatchDBus,
        OsxNotifySystemMismatchNotifySend
      ),
    defaultNotifyTimeout,
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

makeFieldLabelsNoPrefix ''NotifyP

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
  case args ^. #action of
    Disabled -> Nothing
    Without -> case mToml of
      Nothing -> Nothing
      Just toml ->
        Just
          $ MkNotifyP
            { action = toml ^. #action,
              system = mergeNotifySystem (args ^. #system) (toml ^. #system),
              timeout =
                plusDefault
                  defaultNotifyTimeout
                  #timeout
                  (toml ^. #timeout)
            }
    With action -> case mToml of
      Nothing ->
        Just
          $ MkNotifyP
            { action,
              system = mergeNotifySystem (args ^. #system) Nothing,
              timeout = WD.fromWithDisabled (afromInteger 10) (args ^. #timeout)
            }
      Just toml ->
        Just
          $ MkNotifyP
            { action,
              system =
                mergeNotifySystem (args ^. #system) (toml ^. #system),
              timeout =
                plusDefault
                  (afromInteger 10)
                  #timeout
                  (toml ^. #timeout)
            }
  where
    plusDefault :: a -> Lens' NotifyArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

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
