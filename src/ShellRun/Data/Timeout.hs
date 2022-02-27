{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timeout' type.
--
-- @since 0.1.0.0
module ShellRun.Data.Timeout
  ( Timeout (..),
  )
where

import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Prelude

-- | Represents a timeout, which is a non-negative integer.
--
-- @since 0.1.0.0
newtype Timeout = MkTimeout
  { -- | @since 0.1.0.0
    unTimeout :: PosInfNum Natural
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum Timeout

-- | @since 0.1.0.0
instance Ord Timeout where
  compare x y | x == y = EQ
  compare (MkTimeout PPosInf) _ = LT
  compare _ (MkTimeout PPosInf) = GT
  compare (MkTimeout (PFin x)) (MkTimeout (PFin y)) = compare y x

-- | @since 0.1.0.0
instance Bounded Timeout where
  minBound = MkTimeout PPosInf
  maxBound = MkTimeout (PFin 0)

makeFieldLabelsNoPrefix ''Timeout
