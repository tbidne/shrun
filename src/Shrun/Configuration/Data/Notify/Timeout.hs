-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (..),
    parseNotifyTimeout,
    notifyTimeoutStr,
  )
where

import Data.Bits (toIntegralSized)
import Data.Time.Relative qualified as RT
import Data.Word (Word16)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import TOML (Value (Integer, String))

-- | Determines notification timeout.
data NotifyTimeout
  = -- | Times out after the given seconds.
    NotifyTimeoutSeconds Natural
  | -- | Never times out.
    NotifyTimeoutNever
  deriving stock (Eq, Show)

instance Default NotifyTimeout where
  def = NotifyTimeoutSeconds 10

instance FromInteger NotifyTimeout where
  fromZ = NotifyTimeoutSeconds . fromZ

-- DecodeTOML instance does not reuse parseNotifyTimeout as we want to
-- enforce the integer type.

instance DecodeTOML NotifyTimeout where
  tomlDecoder = makeDecoder $ \case
    String t -> parseNotifyTimeoutStr t
    Integer i -> case toIntegralSized i of
      Just i' -> pure $ NotifyTimeoutSeconds i'
      Nothing -> invalidValue (tooLargeErr Nothing) (Integer i)
    badTy -> typeMismatch badTy

instance Pretty NotifyTimeout where
  pretty = \case
    NotifyTimeoutNever -> "off"
    NotifyTimeoutSeconds x -> pretty x

tooLargeErr :: Maybe Integer -> Text
tooLargeErr Nothing = "Timeout integer too large. Max is: " <> showt maxW16
tooLargeErr (Just i) =
  mconcat
    [ "Timeout integer '",
      showt i,
      "' too large. Max is: ",
      showt maxW16
    ]

maxW16 :: Word16
maxW16 = maxBound

-- | Parses 'NotifyTimeout'. For CLI only.
parseNotifyTimeout :: (MonadFail m) => m Text -> m NotifyTimeout
parseNotifyTimeout getTxt = do
  txt <- getTxt
  case U.readStripUnderscores @_ @Natural txt of
    Just nNat -> pure $ NotifyTimeoutSeconds nNat
    Nothing -> parseNotifyTimeoutStr txt
{-# INLINEABLE parseNotifyTimeout #-}

-- | Parses a string only i.e. should either be 'off' or a time string, not
-- a literal natural. Intended for:
--
-- - CLI, after parsing a literal fails.
-- - TOML, when given a String not Integer.
parseNotifyTimeoutStr :: (MonadFail f) => Text -> f NotifyTimeout
parseNotifyTimeoutStr "off" = pure NotifyTimeoutNever
parseNotifyTimeoutStr txt = case RT.fromString str of
  Right n -> pure $ NotifyTimeoutSeconds $ RT.toSeconds n
  Left bad ->
    fail
      $ Utils.fmtUnrecognizedError
        "notify timeout"
        notifyTimeoutStr
        bad
  where
    str = unpack txt
{-# INLINEABLE parseNotifyTimeoutStr #-}

-- | Available 'NotifyTimeout' strings.
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(NATURAL | TIME_STR | off)"
