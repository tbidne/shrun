{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Action
  ( -- * Start
    NotifyActionStartSwitch (..),

    -- * Complete
    NotifyActionComplete (..),
    parseNotifyAction,
    notifyActionStr,
  )
where

import Shrun.Configuration.Data.ConfigPhase (parseSwitch)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
newtype NotifyActionStartSwitch = MkNotifyActionStartSwitch Bool
  deriving stock (Bounded, Eq, Ord, Show)
  deriving newtype (Enum)

instance DecodeTOML NotifyActionStartSwitch where
  tomlDecoder = MkNotifyActionStartSwitch <$> (tomlDecoder >>= parseSwitch)

instance Default NotifyActionStartSwitch where
  def = MkNotifyActionStartSwitch False

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic "unNotifyActionStartSwitch" k NotifyActionStartSwitch NotifyActionStartSwitch a b
  where
  labelOptic = iso (\(MkNotifyActionStartSwitch b) -> b) MkNotifyActionStartSwitch
  {-# INLINE labelOptic #-}

-- | Determines for which 'complete' actions we should send notifications.
data NotifyActionComplete
  = -- | Send a notification after all commands are completed.
    NotifyActionCompleteFinal
  | -- | Send notifications when each command completes.
    NotifyActionCompleteCommand
  | -- | NotifyActionCompleteFinal and NotifyActionCompleteCommand.
    NotifyActionCompleteAll
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML NotifyActionComplete where
  tomlDecoder = tomlDecoder >>= parseNotifyAction

-- | Parses 'NotifyActionComplete'.
parseNotifyAction :: (MonadFail m) => Text -> m NotifyActionComplete
parseNotifyAction = \case
  "final" -> pure NotifyActionCompleteFinal
  "command" -> pure NotifyActionCompleteCommand
  "all" -> pure NotifyActionCompleteAll
  bad ->
    fail
      $ Utils.fmtUnrecognizedError
        "notify action complete"
        notifyActionStr
        (unpack bad)
{-# INLINEABLE parseNotifyAction #-}

-- | Available 'NotifyActionComplete' strings.
notifyActionStr :: (IsString a) => a
notifyActionStr = "all | command | final"
