-- | Provides the 'Timeout' type.
module ShellRun.Data.Timeout
  ( Timeout (..),
  )
where

import ShellRun.Prelude

-- | Represents a timeout, which is a non-negative integer.
newtype Timeout = MkTimeout
  {unTimeout :: RNonNegative}
  deriving (Eq, Ord, Show)
