-- | Provides numerical types for dealing with infinity.
--
-- @since 0.1
module ShellRun.Data.InfNum
  ( PosInfNum (..),
  )
where

import ShellRun.Prelude

-- | Represents a numerical type with positive infinity.
--
-- @since 0.1
data PosInfNum a
  = -- | @since 0.1
    PFin a
  | -- | @since 0.1
    PPosInf
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
