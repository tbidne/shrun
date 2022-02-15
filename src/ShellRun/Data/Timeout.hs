-- | Provides the 'Timeout' type.
--
-- @since 0.1.0.0
module ShellRun.Data.Timeout
  ( Timeout (..),
  )
where

import ShellRun.Prelude

-- | Represents a timeout, which is a non-negative integer.
--
-- @since 0.1.0.0
newtype Timeout = MkTimeout
  { -- | @since 0.1.0.0
    unTimeout :: RNonNegative
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
