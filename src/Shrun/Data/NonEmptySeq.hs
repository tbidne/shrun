{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides the 'NonEmptySeq' type.
--
-- @since 0.1
module Shrun.Data.NonEmptySeq
  ( -- * Type
    NonEmptySeq (..),

    -- * Construction
    singleton,
    mFromList,
    fromNonEmpty,
    unsafeFromList,

    -- * Elimination
    toSeq,
  )
where

import Data.Sequence (Seq (..))
import GHC.Exts (IsList (Item))
import GHC.Exts qualified as Exts
import Shrun.Prelude hiding (toList)

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

-- | @since 0.7
instance IsList (NonEmptySeq a) where
  type Item (NonEmptySeq a) = a
  fromList = unsafeFromList
  toList (x :|^ xs) = Exts.toList (x :<| xs)

-- | @since 0.5
instance DecodeTOML a => DecodeTOML (NonEmptySeq a) where
  tomlDecoder = fromNonEmpty <$> tomlDecoder

-- | Creates a singleton 'NonEmptySeq'.
--
-- @since 0.1
singleton :: a -> NonEmptySeq a
singleton x = x :|^ Empty
{-# INLINEABLE singleton #-}

-- | Exposes the underlying sequence.
--
-- ==== __Examples__
-- >>> toSeq (1 :|^ Exts.fromList [2,3,4])
-- fromList [1,2,3,4]
--
-- @since 0.1
toSeq :: NonEmptySeq a -> Seq a
toSeq (x :|^ xs) = x :<| xs
{-# INLINEABLE toSeq #-}

-- | 'NonEmptySeq' from 'NonEmpty'. Unsafe in the sense that it does not
-- terminate for infinite 'NonEmpty'.
--
-- ==== __Examples__
-- >>> fromNonEmpty ( 1 :| [3,4])
-- 1 :|^ fromList [3,4]
--
-- @since 0.1
fromNonEmpty :: NonEmpty a -> NonEmptySeq a
fromNonEmpty (x :| xs) = x :|^ Exts.fromList xs
{-# INLINEABLE fromNonEmpty #-}

-- | Returns 'Just' 'NonEmptySeq' if the list is non-empty. Otherwise
-- returns 'Nothing'.
--
-- @since 0.5
mFromList :: List a -> Maybe (NonEmptySeq a)
mFromList [] = Nothing
mFromList (x : xs) = Just $ x :|^ Exts.fromList xs

-- | Even more unsafe than 'fromNonEmpty'; will throw an error on an
-- empty list.
--
-- ==== __Examples__
-- >>> [1,2,3]
-- 1 :|^ fromList [2,3]
--
-- @since 0.1
unsafeFromList :: HasCallStack => List a -> NonEmptySeq a
unsafeFromList =
  mFromList .> \case
    Nothing -> error "[Shrun.Data.NonEmptySeq] Empty list passed to unsafeFromList"
    Just xs -> xs

unsafeFromSeq :: HasCallStack => Seq a -> NonEmptySeq a
unsafeFromSeq (x :<| xs) = x :|^ xs
unsafeFromSeq _ = error "[Shrun.Data.NonEmptySeq] Empty seq passed to unsafeFromSeq"
{-# INLINEABLE unsafeFromSeq #-}
