-- | Provides the 'Supremum' type.
module ShellRun.Math.Supremum
  ( Supremum (..),
  )
where

import ShellRun.Prelude

-- | Newtype wrapper for easily deriving 'Semigroup' and 'Monoid' instances.
newtype Supremum a = MkSupremum a
  deriving stock (Bounded, Eq, Ord)

instance Ord a => Semigroup (Supremum a) where
  (<>) = max

instance (Bounded a, Ord a) => Monoid (Supremum a) where
  mempty = minBound
