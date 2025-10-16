{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module. Take care as usage can violate invariants.
module Shrun.Command.Types.Internal
  ( CommandIndex (..),
    fromPositive,
    unsafeFromInt,
    toVertex,
    fromVertex,
    succ,
    range,
  )
where

import Shrun.Prelude

-- | Numeric index for each command, for handling command graph dependencies.
-- Conversion to/from 'Vertex' should use 'indexToVertex' and
-- 'indexFromVertex', as 'CommandIndex' is 1-based whereas 'Vertex' is
-- 0-based.
newtype CommandIndex = MkCommandIndex {unCommandIndex :: Positive Int}
  deriving stock (Show)
  deriving newtype (ASemigroup, Eq, Hashable, MSemigroup, MMonoid, Ord)

instance
  ( k ~ An_Iso,
    a ~ Positive Int,
    b ~ Positive Int
  ) =>
  LabelOptic "unCommandIndex" k CommandIndex CommandIndex a b
  where
  labelOptic = iso (\(MkCommandIndex i) -> i) MkCommandIndex
  {-# INLINE labelOptic #-}

succ :: CommandIndex -> CommandIndex
succ = (.+. one)

range :: CommandIndex -> CommandIndex -> Either String (NESeq CommandIndex)
range (MkCommandIndex (MkPositive lower)) (MkCommandIndex (MkPositive upper)) =
  if lower <= upper
    then Right $ MkCommandIndex . unsafePositive <$> lower :<|| [lower + 1 .. upper]
    else
      Left
        $ mconcat
          [ "Bad range. Expected ",
            show lower,
            " <= ",
            show upper
          ]

fromPositive :: Positive Int -> CommandIndex
fromPositive = MkCommandIndex

unsafeFromInt :: (HasCallStack) => Int -> CommandIndex
unsafeFromInt = fromPositive . unsafePositive

-- | Conversion to 'Vertex'.
toVertex :: CommandIndex -> Vertex
toVertex (MkCommandIndex (MkPositive i)) = i - 1

-- | Conversion from 'Vertex'.
fromVertex :: (HasCallStack) => Vertex -> CommandIndex
fromVertex v = MkCommandIndex (unsafePositive $ v + 1)
