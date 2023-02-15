{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
--
-- @since 0.8
module Shrun.Data.PollInterval
  ( PollInterval (..),
    defaultPollInterval,
  )
where

import Shrun.Prelude
import TOML (Value (Integer))

-- | Represents how often to poll for service changes, in microseconds.
-- Zero is interpreted as infinite i.e. limited only by the CPU.
--
-- @since 0.8
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

-- | @since 0.8
makeFieldLabelsNoPrefix ''PollInterval

-- | @since 0.8
instance DecodeTOML PollInterval where
  tomlDecoder = makeDecoder $ \case
    Integer i
      | i >= 0 -> pure $ MkPollInterval $ fromIntegral i
      | otherwise ->
          invalidValue
            "Unexpected poll-interval. Integer must be >= 0."
            (Integer i)
    badTy -> typeMismatch badTy

-- | @since 0.8
defaultPollInterval :: PollInterval
defaultPollInterval = MkPollInterval 10_000
