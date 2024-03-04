{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
module Shrun.Data.PollInterval
  ( PollInterval (..),
    parsePollInterval,
    defaultPollInterval,
  )
where

import Shrun.Prelude

-- | Represents how often to poll for command logs, in microseconds.
-- Zero is interpreted as infinite i.e. limited only by the CPU.
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

makeFieldLabelsNoPrefix ''PollInterval

instance DecodeTOML PollInterval where
  tomlDecoder = MkPollInterval <$> tomlDecoder

parsePollInterval :: (Functor m) => m Natural -> m PollInterval
parsePollInterval getNat = MkPollInterval <$> getNat

-- | Default 'PollInterval'.
--
-- >>> defaultPollInterval
-- MkPollInterval {unPollInterval = 10000}
defaultPollInterval :: PollInterval
defaultPollInterval = MkPollInterval 10_000
