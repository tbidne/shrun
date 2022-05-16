{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'NonEmptySeq' type.
--
-- @since 0.1
module ShellRun.Data.NonEmptySeq
  ( -- * Type
    NonEmptySeq (..),

    -- * Construction
    singleton,

    -- * Elimination
    toSeq,
    toList,

    -- * Unsafe
    unsafeFromNonEmpty,
    unsafeFromList,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq (..))
import GHC.Exts qualified as Exts
import ShellRun.Prelude hiding (toList)

-- $setup
-- >>> import GHC.Exts (fromList)

-- | Represents a non-empty sequence. This is useful for when we want a
-- non-empty, finite list.
--
-- @since 0.1
data NonEmptySeq a = a :|^ (Seq a)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

infixr 5 :|^

makePrismLabels ''NonEmptySeq

-- | @since 0.1
instance Semigroup (NonEmptySeq a) where
  xs <> ys = unsafeFromSeq $ toSeq xs <> toSeq ys
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Functor NonEmptySeq where
  fmap f = unsafeFromSeq . fmap f . toSeq
  {-# INLINEABLE fmap #-}

-- | @since 0.1
instance Applicative NonEmptySeq where
  pure x = x :|^ Empty
  f <*> xs = unsafeFromSeq $ toSeq f <*> toSeq xs
  {-# INLINEABLE (<*>) #-}

-- | @since 0.1
instance Monad NonEmptySeq where
  xs >>= f = unsafeFromSeq $ toSeq xs >>= (toSeq . f)
  {-# INLINEABLE (>>=) #-}

-- | @since 0.1
instance Foldable NonEmptySeq where
  foldMap f = foldMap f . toSeq
  {-# INLINEABLE foldMap #-}

-- | @since 0.1
instance Traversable NonEmptySeq where
  traverse f = fmap unsafeFromSeq . traverse f . toSeq
  {-# INLINEABLE traverse #-}

-- | Creates a singleton 'NonEmptySeq'.
--
-- @since 0.1
singleton :: a -> NonEmptySeq a
singleton x = x :|^ Empty
{-# INLINEABLE singleton #-}

-- | Exposes the underlying sequence.
--
-- ==== __Examples__
-- >>> toSeq (1 :|^ fromList [2,3,4])
-- fromList [1,2,3,4]
--
-- @since 0.1
toSeq :: NonEmptySeq a -> Seq a
toSeq (x :|^ xs) = x :<| xs
{-# INLINEABLE toSeq #-}

-- | 'NonEmptySeq' to list.
--
-- ==== __Examples__
-- >>> toList (1 :|^ fromList [2,3,4])
-- [1,2,3,4]
--
-- @since 0.1
toList :: NonEmptySeq a -> List a
toList (x :|^ xs) = Exts.toList (x :<| xs)
{-# INLINEABLE toList #-}

-- | 'NonEmptySeq' from 'NonEmpty'. Unsafe in the sense that it does not
-- terminate for infinite 'NonEmpty'.
--
-- ==== __Examples__
-- >>> unsafeFromNonEmpty ( 1 :| [3,4])
-- 1 :|^ fromList [3,4]
--
-- @since 0.1
unsafeFromNonEmpty :: HasCallStack => NonEmpty a -> NonEmptySeq a
unsafeFromNonEmpty (x :| xs) = x :|^ Exts.fromList xs
{-# INLINEABLE unsafeFromNonEmpty #-}

-- | Even more unsafe than 'unsafeFromNonEmpty'; will throw an error on an
-- empty list.
--
-- ==== __Examples__
-- >>> unsafeFromList [1,2,3]
-- 1 :|^ fromList [2,3]
--
-- @since 0.1
unsafeFromList :: HasCallStack => List a -> NonEmptySeq a
unsafeFromList [] = error "[ShellRun.Data.NonEmptySeq] Empty list passed to unsafeFromList"
unsafeFromList (x : xs) = x :|^ Exts.fromList xs
{-# INLINEABLE unsafeFromList #-}

unsafeFromSeq :: HasCallStack => Seq a -> NonEmptySeq a
unsafeFromSeq (x :<| xs) = x :|^ xs
unsafeFromSeq _ = error "[ShellRun.Data.NonEmptySeq] Empty seq passed to unsafeFromSeq"
{-# INLINEABLE unsafeFromSeq #-}
