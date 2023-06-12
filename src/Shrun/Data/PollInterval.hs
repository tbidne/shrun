{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
module Shrun.Data.PollInterval
  ( PollInterval (..),
    defaultPollInterval,
  )
where

import Shrun.Prelude
import TOML (Value (Integer))

-- | Represents how often to poll for command logs, in microseconds.
-- Zero is interpreted as infinite i.e. limited only by the CPU.
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

makeFieldLabelsNoPrefix ''PollInterval

instance DecodeTOML PollInterval where
  tomlDecoder = makeDecoder $ \case
    Integer i
      | i >= 0 -> pure $ MkPollInterval $ fromIntegral i
      | otherwise ->
          invalidValue
            "Unexpected poll-interval. Integer must be >= 0."
            (Integer i)
    badTy -> typeMismatch badTy

-- | Default 'PollInterval'.
--
-- >>> defaultPollInterval
-- MkPollInterval {unPollInterval = 10000}
defaultPollInterval :: PollInterval
defaultPollInterval = MkPollInterval 10_000
