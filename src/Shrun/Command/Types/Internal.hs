{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module. Take care as usage can violate invariants.
module Shrun.Command.Types.Internal
  ( -- * Index
    CommandIndex (..),
    fromPositive,
    unsafeFromInt,
    succ,
    addNN,
    range,

    -- * Vertex
    Vertex,
    LVertex,
    toVertex,
    fromVertex,
  )
where

import Numeric.Data.Positive.Internal (Positive (UnsafePositive))
import Shrun.Prelude

-- | Numeric index for each command, for handling command graph dependencies.
-- Conversion to/from 'Vertex' should use 'indexToVertex' and
-- 'indexFromVertex', as 'CommandIndex' is 1-based whereas 'Vertex' is
-- 0-based.
newtype CommandIndex = MkCommandIndex {unCommandIndex :: Positive Int}
  deriving stock (Generic, Show)
  deriving newtype (ASemigroup, Eq, Hashable, MSemigroup, MMonoid, Ord)
  deriving anyclass (NFData)
  deriving (Pretty) via Int

instance Enum CommandIndex where
  toEnum = unsafeFromInt

  fromEnum = view (#unCommandIndex % #unPositive)

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

addNN :: CommandIndex -> NonNegative Int -> CommandIndex
addNN idx = MkCommandIndex . unsafePositive . (i +) . view #unNonNegative
  where
    i = idx ^. #unCommandIndex % #unPositive

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

-- | Type for Command graph vertex, for usage with fgl.
type Vertex = Int

-- | Labeled 'Vertex'.
type LVertex a = Tuple2 Vertex a

-- | Conversion to 'Vertex'.
toVertex :: CommandIndex -> Vertex
toVertex (MkCommandIndex (MkPositive i)) = i

-- | Conversion from 'Vertex'.
fromVertex :: (HasCallStack) => Vertex -> CommandIndex
fromVertex v = MkCommandIndex (unsafePositive v)
