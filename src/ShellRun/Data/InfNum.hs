-- | Provides numerical types for dealing with infinite.
--
-- @since 0.1.0.0
module ShellRun.Data.InfNum
  ( PosInfNum (..),
  )
where

import ShellRun.Prelude

-- | Represents a numerical type with positive infinity.
--
-- @since 0.1.0.0
data PosInfNum a
  = -- | @since 0.1.0.0
    PFin a
  | -- | @since 0.1.0.0
    PPosInf
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
