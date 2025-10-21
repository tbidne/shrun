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
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction,
  )
import Shrun.Configuration.Data.Notify.System
  ( LinuxNotifySystemMismatch (LinuxNotifySystemMismatchAppleScript),
    NotifySystemEnv,
    NotifySystemP (AppleScript, DBus, NotifySend),
    OsxNotifySystemMismatch
      ( OsxNotifySystemMismatchDBus,
        OsxNotifySystemMismatchNotifySend
      ),
    displayNotifySystem,
    mergeNotifySystem,
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With),
    (<|?|>),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default, def, (<.>))
import Shrun.Notify.DBus (MonadDBus (connectSession))
import Shrun.Prelude

-- See NOTE: [Args vs. Toml mandatory fields]

-- | Notify action is mandatory if we are running notifications.
type NotifyActionF :: ConfigPhase -> Type
type family NotifyActionF p where
  NotifyActionF ConfigPhaseArgs = Maybe (WithDisabled NotifyAction)
  NotifyActionF ConfigPhaseToml = Maybe (WithDisabled NotifyAction)
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
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP b a2 a3)
          (f a1)
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
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 b a3)
          (f a2)
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
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance Semigroup NotifyToml where
  l <> r =
    MkNotifyP
      { action = l ^. #action <|> r ^. #action,
        system = l ^. #system <|> r ^. #system,
        timeout = l ^. #timeout <|> r ^. #timeout
      }

instance Monoid NotifyToml where
  mempty =
    MkNotifyP
      { action = Nothing,
        system = Nothing,
        timeout = Nothing
      }

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

-- Only Default instance is for Args, since others require the action.
instance Default NotifyArgs where
  def =
    MkNotifyP
      { system = Nothing,
        action = Nothing,
        timeout = Nothing
      }

-- | Merges args and toml configs.
mergeNotifyLogging ::
  NotifyArgs ->
  Maybe NotifyToml ->
  Maybe NotifyMerged
mergeNotifyLogging args mToml =
  mAction <&> \action ->
    let toml :: NotifyToml
        toml = fromMaybe defaultNotifyToml mToml
     in MkNotifyP
          { action,
            system =
              mergeNotifySystem (args ^. #system) (toml ^. #system),
            timeout =
              (args ^. #timeout) <.> (toml ^. #timeout)
          }
  where
    mAction :: Maybe NotifyAction
    mAction = args ^. #action <|?|> (mToml ^? _Just % #action % _Just)

instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyP
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"

#if OSX

toEnv ::
  ( HasCallStack,
    MonadThrow m
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
  ( HasCallStack,
    MonadDBus m,
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

{-# INLINEABLE toEnv #-}

mkNotify :: NotifyMerged -> NotifySystemEnv -> NotifyEnv
mkNotify notifyToml systemP2 =
  MkNotifyP
    { system = systemP2,
      action = notifyToml ^. #action,
      timeout = notifyToml ^. #timeout
    }

defaultNotifyToml :: NotifyToml
defaultNotifyToml =
  MkNotifyP
    { system = Nothing,
      action = Nothing,
      timeout = Nothing
    }
