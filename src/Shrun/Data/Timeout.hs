{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timeout' type.
--
-- @since 0.1
module Shrun.Data.Timeout
  ( Timeout (..),
  )
where

import Data.Time.Relative qualified as RT
import Shrun.Prelude
import TOML (Value (Integer, String))
import Text.Read (readMaybe)

-- | Represents a timeout, which is a non-negative integer.
--
-- @since 0.1
newtype Timeout = MkTimeout
  { -- | @since 0.1
    unTimeout :: Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.5
      Num
    )
    via Natural

-- | @since 0.6.1
makeFieldLabelsNoPrefix ''Timeout

-- | @since 0.5
instance DecodeTOML Timeout where
  tomlDecoder = makeDecoder $ \case
    -- Valid strings are "time string" e.g. "1d4h" only. If literal seconds
    -- supplied, we want them to be explicit integer types.
    String s ->
      unpack s & \s' -> do
        case fromTimeString s' of
          Nothing ->
            invalidValue
              "Unexpected timeout. Only valid strings are time strings."
              (String s)
          Just n -> pure $ MkTimeout $ RT.toSeconds n
    Integer i
      | i >= 0 -> pure $ MkTimeout $ fromIntegral i
      | otherwise ->
          invalidValue
            "Unexpected timeout. Integer must be >= 0."
            (Integer i)
    badTy -> typeMismatch badTy
    where
      -- parses a RelativeTime from a "time string" if the string is _not_
      -- a valid integer (e.g. "30").
      fromTimeString s =
        fmap
          (\_ -> RT.fromString s)
          (readMaybe @Integer s ^? _Nothing)
          ^? (_Just % _Right)
