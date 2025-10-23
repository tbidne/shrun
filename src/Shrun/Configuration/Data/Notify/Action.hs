-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Action
  ( NotifyAction (..),
    parseNotifyAction,
    notifyActionStr,
  )
where

import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Determines for which actions we should send notifications.
data NotifyAction
  = -- | Send a notification after all commands are completed.
    NotifyFinal
  | -- | Send notifications when each command completes.
    NotifyCommand
  | -- | NotifyFinal and NotifyCommand.
    NotifyAll
  deriving stock (Bounded, Enum, Eq, Show)

instance DecodeTOML NotifyAction where
  tomlDecoder = tomlDecoder >>= parseNotifyAction

-- | Parses 'NotifyAction'.
parseNotifyAction :: (MonadFail m) => Text -> m NotifyAction
parseNotifyAction = \case
  "final" -> pure NotifyFinal
  "command" -> pure NotifyCommand
  "all" -> pure NotifyAll
  bad ->
    fail
      $ Utils.fmtUnrecognizedError
        "notify action"
        notifyActionStr
        (unpack bad)
{-# INLINEABLE parseNotifyAction #-}

-- | Available 'NotifyAction' strings.
notifyActionStr :: (IsString a) => a
notifyActionStr = "all | command | final"
