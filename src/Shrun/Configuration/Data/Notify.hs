{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Notify
  ( NotifyP (..),
    NotifyArgs,
    NotifyToml,
    NotifyMerged,
    mergeNotifyLogging,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.WithDisable
  ( WithDisable (Disabled, With),
    alternativeDefault,
    defaultIfDisabled,
  )
import Shrun.Notify.Types
  ( NotifyAction,
    NotifySystemP1,
    NotifyTimeout,
    defaultNotifySystem,
  )
import Shrun.Prelude

-- See NOTE: [Args vs. Toml mandatory fields]

-- | Notify action is mandatory if we are running notifications.
type NotifyActionF :: ConfigPhase -> Type
type family NotifyActionF p where
  NotifyActionF ConfigPhaseArgs = WithDisable (Maybe NotifyAction)
  NotifyActionF ConfigPhaseToml = NotifyAction
  NotifyActionF ConfigPhaseMerged = NotifyAction

-- | Holds notification config.
type NotifyP :: ConfigPhase -> Type
data NotifyP p = MkNotifyP
  { -- | Actions for which to send notifications.
    action :: NotifyActionF p,
    -- | The notification system to use.
    system :: ConfigPhaseF p NotifySystemP1,
    -- | when to timeout successful notifications.
    timeout :: ConfigPhaseF p NotifyTimeout
  }

makeFieldLabelsNoPrefix ''NotifyP

type NotifyArgs = NotifyP ConfigPhaseArgs

type NotifyToml = NotifyP ConfigPhaseToml

type NotifyMerged = NotifyP ConfigPhaseMerged

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
    -- 1. Notifications globally disabled
    Disabled -> Nothing
    With mAction -> case (mAction, mToml) of
      -- 2. Neither Args nor Toml specifies notifications
      (Nothing, Nothing) -> Nothing
      -- 3. Args but no Toml
      (Just argsAction, Nothing) ->
        Just
          $ MkNotifyP
            { action = argsAction,
              system = defaultIfDisabled defaultNotifySystem (args ^. #system),
              timeout = defaultIfDisabled (afromInteger 10) (args ^. #timeout)
            }
      (mArgsAction, Just toml) ->
        Just
          $ MkNotifyP
            { action = fromMaybe (toml ^. #action) mArgsAction,
              system = altDefault defaultNotifySystem #system (toml ^. #system),
              timeout = altDefault (afromInteger 10) #timeout (toml ^. #timeout)
            }
  where
    altDefault :: a -> Lens' NotifyArgs (WithDisable (Maybe a)) -> Maybe a -> a
    altDefault defA l = alternativeDefault defA (args ^. l)

instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyP
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"
