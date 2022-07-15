{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timeout' type.
--
-- @since 0.1
module ShellRun.Data.Timeout
  ( Timeout (..),
  )
where

import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Prelude

-- | Represents a timeout, which is a non-negative integer.
--
-- @since 0.1
newtype Timeout = MkTimeout
  { -- | @since 0.1
    unTimeout :: PosInfNum Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum Timeout

-- | @since 0.1
instance Ord Timeout where
  compare x y | x == y = EQ
  compare (MkTimeout PPosInf) _ = LT
  compare _ (MkTimeout PPosInf) = GT
  compare (MkTimeout (PFin x)) (MkTimeout (PFin y)) = compare y x
  {-# INLINEABLE compare #-}

-- | @since 0.1
instance Bounded Timeout where
  minBound = MkTimeout PPosInf
  {-# INLINEABLE minBound #-}
  maxBound = MkTimeout (PFin 0)
  {-# INLINEABLE maxBound #-}

-- | @since 0.4.0.1
makePrisms ''Timeout
