{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Supremum' type.
--
-- @since 0.1.0.0
module ShellRun.Data.Supremum
  ( Supremum (..),
  )
where

import ShellRun.Prelude

-- | Newtype wrapper for easily deriving 'Semigroup' and 'Monoid' instances.
--
-- @since 0.1.0.0
newtype Supremum a = MkSupremum a
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord
    )

-- | @since 0.1.0.0
instance Ord a => Semigroup (Supremum a) where
  (<>) :: Supremum a -> Supremum a -> Supremum a
  (<>) = max

-- | @since 0.1.0.0
instance (Bounded a, Ord a) => Monoid (Supremum a) where
  mempty :: Supremum a
  mempty = minBound

makePrismLabels ''Supremum
