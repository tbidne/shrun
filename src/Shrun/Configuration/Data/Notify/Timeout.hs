-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (..),
    parseNotifyTimeout,
    notifyTimeoutStr,
  )
where

import Data.Bits (toIntegralSized)
import Data.Word (Word16)
import GHC.Num (Num (fromInteger))
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as U
import TOML (Value (Integer, String))

-- | Determines notification timeout.
data NotifyTimeout
  = -- | Times out after the given seconds.
    NotifyTimeoutSeconds Word16
  | -- | Never times out.
    NotifyTimeoutNever
  deriving stock (Eq, Show)

instance Default NotifyTimeout where
  def = NotifyTimeoutSeconds 10

instance FromInteger NotifyTimeout where
  afromInteger = NotifyTimeoutSeconds . fromInteger

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
parseNotifyTimeout :: (MonadFail m) => m Text -> m NotifyTimeout
parseNotifyTimeout getTxt =
  getTxt >>= \case
    "never" -> pure NotifyTimeoutNever
    other -> NotifyTimeoutSeconds <$> U.readStripUnderscores other
{-# INLINEABLE parseNotifyTimeout #-}

-- | Available 'NotifyTimeout' strings.
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(never | NATURAL)"
