-- | Provides the 'Supremum' type.
module ShellRun.Data.Supremum
  ( Supremum (..),
  )
where

import ShellRun.Prelude

-- | Newtype wrapper for easily deriving 'Semigroup' and 'Monoid' instances.
newtype Supremum a = MkSupremum a
  deriving stock (Bounded, Eq, Ord)

instance Ord a => Semigroup (Supremum a) where
  (<>) :: Supremum a -> Supremum a -> Supremum a
  (<>) = max

instance (Bounded a, Ord a) => Monoid (Supremum a) where
  mempty :: Supremum a
  mempty = minBound