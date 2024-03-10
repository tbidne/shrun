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
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
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
  NotifyActionF ConfigPhaseArgs = WithDisabled NotifyAction
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
    Without -> case mToml of
      Nothing -> Nothing
      Just toml ->
        Just
          $ MkNotifyP
            { action = toml ^. #action,
              system =
                plusDefault
                  defaultNotifySystem
                  #system
                  (toml ^. #system),
              timeout =
                plusDefault
                  (afromInteger 10)
                  #timeout
                  (toml ^. #timeout)
            }
    With action -> case mToml of
      -- 3. Args but no Toml
      Nothing ->
        Just
          $ MkNotifyP
            { action,
              system = WD.fromWithDisabled defaultNotifySystem (args ^. #system),
              timeout = WD.fromWithDisabled (afromInteger 10) (args ^. #timeout)
            }
      Just toml ->
        Just
          $ MkNotifyP
            { action,
              system =
                plusDefault
                  defaultNotifySystem
                  #system
                  (toml ^. #system),
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
