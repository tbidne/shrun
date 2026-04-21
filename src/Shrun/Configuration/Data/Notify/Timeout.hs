-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Timeout
  ( parseNotifyTimeout,
    notifyTimeoutMeta,
    notifyTimeoutDecoder,
    prettyNotifyTimeout,
  )
where

import Data.Bits (toIntegralSized)
import Data.Time.Relative qualified as RT
import Data.Word (Word16)
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import TOML (Value (Integer, String))

-- DecodeTOML instance does not reuse parseNotifyTimeout as we want to
-- enforce the integer type.

notifyTimeoutDecoder :: Decoder NotifyTimeout
notifyTimeoutDecoder = makeDecoder $ \case
  String t -> parseNotifyTimeoutStr t
  Integer i -> case toIntegralSized i of
    Just i' -> pure $ NotifyTimeoutMillis $ i' * 1_000
    Nothing -> invalidValue (tooLargeErr Nothing) (Integer i)
  badTy -> typeMismatch badTy

prettyNotifyTimeout :: NotifyTimeout -> Doc ann
prettyNotifyTimeout = \case
  NotifyTimeoutNever -> "off"
  NotifyTimeoutMillis x -> pretty $ x `div` 1_000

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
    Just nNat -> pure $ NotifyTimeoutMillis $ unsafeFromNatSec nNat
    Nothing -> parseNotifyTimeoutStr txt
{-# INLINEABLE parseNotifyTimeout #-}

unsafeFromNatSec :: Natural -> Int
unsafeFromNatSec = unsafeConvertIntegral . (* 1_000)

-- | Parses a string only i.e. should either be 'off' or a time string, not
-- a literal natural. Intended for:
--
-- - CLI, after parsing a literal fails.
-- - TOML, when given a String not Integer.
parseNotifyTimeoutStr :: (MonadFail f) => Text -> f NotifyTimeout
parseNotifyTimeoutStr "off" = pure NotifyTimeoutNever
parseNotifyTimeoutStr txt = case RT.fromString str of
  Right n -> pure $ NotifyTimeoutMillis $ unsafeFromNatSec $ RT.toSeconds n
  Left bad ->
    fail
      $ Utils.fmtUnrecognizedError
        "notify timeout"
        notifyTimeoutMeta
        bad
  where
    str = unpack txt
{-# INLINEABLE parseNotifyTimeoutStr #-}

notifyTimeoutMeta :: (IsString a) => Tuple2 Bool (List a)
notifyTimeoutMeta = (True, ["NATURAL", "TIME_STR"])
