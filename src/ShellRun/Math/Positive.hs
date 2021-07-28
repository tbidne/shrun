-- | Provides the 'Positive' type for safe mathematical
-- operations.
module ShellRun.Math.Positive
  ( Positive,
    getPositive,
    mkPositive,
    unsafePositive,
  )
where

import ShellRun.Prelude

-- | Newtype wrapper over 'Int'.
newtype Positive = MkPositive
  { -- | Unwraps the 'Positive'
    getPositive :: Int
  }
  deriving (Eq, Ord, Show)

-- | Smart constructor for 'Positive'.
--
-- Examples:
--
-- >>> mkPositive 7
-- Just (MkPositive {getPositive = 7})
--
-- >>> mkPositive 0
-- Nothing
mkPositive :: Int -> Maybe Positive
mkPositive n
  | n > 0 = Just $ MkPositive n
  | otherwise = Nothing

-- | Unsafe constructor for 'Positive', intended to be used with
-- known constants, e.g., @unsafePositive 7@. Exercise restraint!
unsafePositive :: Int -> Positive
unsafePositive n
  | n > 0 = MkPositive n
  | otherwise =
    error $
      "Passed non-positive "
        <> showt n
        <> " to unsafePositive!"
