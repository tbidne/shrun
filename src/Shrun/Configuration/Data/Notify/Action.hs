-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Action
  ( NotifyActionComplete (..),
    parseNotifyAction,
    notifyActionStr,
  )
where

import Shrun.Prelude
import Shrun.Utils qualified as Utils

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
