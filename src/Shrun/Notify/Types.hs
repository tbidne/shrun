{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for notifications.
--
-- @since X.X
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
--
-- @since X.X
data NotifyAction
  = -- | Do not send any notifications.
    --
    -- @since X.X
    NotifyNone
  | -- | Send a notification after all commands are completed.
    --
    -- @since X.X
    NotifyFinal
  | -- | Send notifications when each command completes. Implies
    -- 'NotifyFinal'.
    --
    -- @since X.X
    NotifyCommand
  deriving stock
    ( -- | @since X.X
      Eq,
      -- | @since X.X
      Show
    )

-- | @since X.X
makePrisms ''NotifyAction

-- | @since X.X
instance DecodeTOML NotifyAction where
  tomlDecoder = tomlDecoder >>= parseNotifyAction

-- | Parses 'NotifyAction'.
--
-- @since X.X
parseNotifyAction :: (MonadFail m) => Text -> m NotifyAction
parseNotifyAction "none" = pure NotifyNone
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
--
-- @since X.X
notifyActionStr :: (IsString a) => a
notifyActionStr = "(none|final|command)"

-- | Maps DBus to its phased param.
--
-- @since X.X
type family DBusF p where
  DBusF Phase1 = ()
  DBusF Phase2 = Client

-- | @since X.X
type NotifySystemP1 = NotifySystem Phase1

-- | @since X.X
type NotifySystemP2 = NotifySystem Phase2

-- | Notification systems.
--
-- @since X.X
type NotifySystem :: Phase -> Type
data NotifySystem p
  = -- | Uses DBus.
    --
    -- @since X.X
    DBus !(DBusF p)
  | -- | Uses notify-send.
    --
    -- @since X.X
    NotifySend

-- | @since X.X
deriving stock instance Eq (NotifySystem Phase1)

-- | @since X.X
deriving stock instance Show (NotifySystem Phase1)

-- | @since X.X
instance DecodeTOML (NotifySystem Phase1) where
  tomlDecoder = tomlDecoder >>= parseNotifySystem

-- | @since X.X
instance AdvancePhase (NotifySystem Phase1) where
  type NextPhase (NotifySystem Phase1) = Either (NotifySystem Phase2) (Client -> NotifySystem Phase2)
  advancePhase NotifySend = Left NotifySend
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
--
-- @since X.X
parseNotifySystem :: (MonadFail m) => Text -> m (NotifySystem Phase1)
parseNotifySystem "dbus" = pure $ DBus ()
parseNotifySystem "notify-send" = pure NotifySend
parseNotifySystem other =
  fail $
    mconcat
      [ "Unrecognized notify system: '",
        T.unpack other,
        "'. Expected one of ",
        notifySystemStr
      ]

-- | Available 'NotifySystem' strings.
--
-- @since X.X
notifySystemStr :: (IsString a) => a
notifySystemStr = "(dbus|notify-send)"

-- | Determines notification timeout.
--
-- @since X.X
data NotifyTimeout
  = -- | Times out after the given seconds.
    --
    -- @since X.X
    NotifyTimeoutSeconds !Word16
  | -- | Never times out.
    --
    -- @since X.X
    NotifyTimeoutNever
  deriving stock
    ( -- | @since X.X
      Eq,
      -- | @since X.X
      Show
    )

-- DecodeTOML instance does not reuse parseNotifyTimeout as we want to
-- enforce the integer type.

-- | @since X.X
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
--
-- @since X.X
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
--
-- @since X.X
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(never|NAT)"

-- | Holds notification config.
--
-- @since X.X
data NotifyConfig = MkNotifyConfig
  { -- | Notification action.
    --
    -- @since X.X
    action :: !NotifyAction,
    -- | Timeout to use for notifications.
    --
    -- @since X.X
    timeout :: !NotifyTimeout
  }
  deriving stock
    ( -- | @since X.X
      Eq,
      -- | @since X.X
      Show
    )

-- | @since X.X
makeFieldLabelsNoPrefix ''NotifyConfig
