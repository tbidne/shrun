{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for notifications.
module Shrun.Notify.Types
  ( -- * Main type
    NotifyConfig (..),

    -- * Notify system
    NotifySystem (..),
    parseNotifySystem,
    notifySystemStr,
    NotifySystemP1,
    NotifySystemP2,
    DBusF,

    -- * Notify actions
    NotifyAction (..),
    parseNotifyAction,
    notifyActionStr,

    -- ** Optics
    _NotifyNone,
    _NotifyFinal,
    _NotifyCommand,

    -- * Notify timeout
    NotifyTimeout (..),
    parseNotifyTimeout,
    notifyTimeoutStr,
  )
where

import DBus.Client (Client)
import Data.Bits (toIntegralSized)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Word (Word16)
import Shrun.Data.Phase (AdvancePhase (..), Phase (..))
import Shrun.Prelude
import TOML (Value (..))
import Text.Read qualified as TR

-- | Determines for which actions we should send notifications.
data NotifyAction
  = -- | Do not send any notifications.
    NotifyNone
  | -- | Send a notification after all commands are completed.
    NotifyFinal
  | -- | Send notifications when each command completes. Implies
    -- 'NotifyFinal'.
    NotifyCommand
  deriving stock (Eq, Show)

makePrisms ''NotifyAction

instance DecodeTOML NotifyAction where
  tomlDecoder = tomlDecoder >>= parseNotifyAction

-- | Parses 'NotifyAction'.
parseNotifyAction :: (MonadFail m) => Text -> m NotifyAction
parseNotifyAction "final" = pure NotifyFinal
parseNotifyAction "command" = pure NotifyCommand
parseNotifyAction other =
  fail $
    mconcat
      [ "Unrecognized notify action: '",
        T.unpack other,
        "'. Expected one of ",
        notifyActionStr
      ]

-- | Available 'NotifyAction' strings.
notifyActionStr :: (IsString a) => a
notifyActionStr = "(final|command)"

-- | Maps DBus to its phased param.
type family DBusF p where
  DBusF Phase1 = ()
  DBusF Phase2 = Client

type NotifySystemP1 = NotifySystem Phase1

type NotifySystemP2 = NotifySystem Phase2

-- | Notification systems.
type NotifySystem :: Phase -> Type
data NotifySystem p
  = -- | Uses DBus.
    DBus !(DBusF p)
  | -- | Uses notify-send.
    NotifySend
  | -- | Uses apple-script.
    AppleScript

deriving stock instance Eq (NotifySystem Phase1)

deriving stock instance Show (NotifySystem Phase1)

instance DecodeTOML (NotifySystem Phase1) where
  tomlDecoder = tomlDecoder >>= parseNotifySystem

instance AdvancePhase (NotifySystem Phase1) where
  type NextPhase (NotifySystem Phase1) = Either (NotifySystem Phase2) (Client -> NotifySystem Phase2)
  advancePhase NotifySend = Left NotifySend
  advancePhase AppleScript = Left AppleScript
  advancePhase (DBus _) = Right DBus

-- NOTE: It would be nice if we could guarantee that the above is "doing the
-- right thing" i.e. NotifySend -> NotifySend and DBus -> DBus, but that
-- would require dependent types e.g. (dependent visible kinds for clarity):
--
--     type ConsNS :: forall k -> k -> Type
--     type family ConsNS l k where
--       ConsNS (NotifySystem p) NotifySend = ()
--       ConsNS (NotifySystem p) (DBus _) = Client
--
--     advancePhase :: foreach (x :: NotifySystem Phase1) -> ConsNS (NotifySystem Phase1) x -> NotifySystem Phase2
--
--     -- call this like
--     advancePhase NotifySend ()
--     advancePhase (DBus ()) client
--
-- That is, we want to pick the _type_ of the "extra" parameter based on the
-- _value_ of the first parameter.

-- | Parses 'NotifySystem'.
parseNotifySystem :: (MonadFail m) => Text -> m (NotifySystem Phase1)
parseNotifySystem "dbus" = pure $ DBus ()
parseNotifySystem "notify-send" = pure NotifySend
parseNotifySystem "apple-script" = pure AppleScript
parseNotifySystem other =
  fail $
    mconcat
      [ "Unrecognized notify system: '",
        T.unpack other,
        "'. Expected one of ",
        notifySystemStr
      ]

-- | Available 'NotifySystem' strings.
notifySystemStr :: (IsString a) => a
notifySystemStr = "(dbus|notify-send|apple-script)"

-- | Determines notification timeout.
data NotifyTimeout
  = -- | Times out after the given seconds.
    NotifyTimeoutSeconds !Word16
  | -- | Never times out.
    NotifyTimeoutNever
  deriving stock (Eq, Show)

-- DecodeTOML instance does not reuse parseNotifyTimeout as we want to
-- enforce the integer type.

instance DecodeTOML NotifyTimeout where
  tomlDecoder = makeDecoder $ \case
    String "never" -> pure NotifyTimeoutNever
    String bad -> invalidValue strErr (String bad)
    Integer i -> case toIntegralSized i of
      Just i' -> pure $ NotifyTimeoutSeconds i'
      Nothing -> invalidValue tooLargeErr (Integer i)
    badTy -> typeMismatch badTy
    where
      tooLargeErr = "Timeout integer too large. Max is: " <> showt maxW16
      strErr = "Unexpected timeout. Only valid string is 'never'."
      maxW16 = maxBound @Word16

-- | Parses 'NotifyTimeout'.
parseNotifyTimeout :: (MonadFail m) => Text -> m NotifyTimeout
parseNotifyTimeout "never" = pure NotifyTimeoutNever
parseNotifyTimeout other = case TR.readMaybe other' of
  Just n -> pure $ NotifyTimeoutSeconds n
  Nothing ->
    fail $
      mconcat
        [ "Unrecognized notify timeout: '",
          other',
          "'. Expected one of ",
          notifyTimeoutStr
        ]
  where
    other' = T.unpack other

-- | Available 'NotifyTimeout' strings.
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(never|NAT)"

-- | Holds notification config.
data NotifyConfig = MkNotifyConfig
  { -- | Notification action.
    action :: !NotifyAction,
    -- | Timeout to use for notifications.
    timeout :: !NotifyTimeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''NotifyConfig
