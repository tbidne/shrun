-- | Provides the 'NonNegative' type for safe mathematical
-- operations.
module ShellRun.Math.NonNegative
  ( NonNegative,
    getNonNegative,
    mkNonNegative,
    unsafeNonNegative,
  )
where

import ShellRun.Prelude

-- | Newtype wrapper over 'Int'.
newtype NonNegative = MkNonNegative
  { -- | Unwraps the 'NonNegative'
    getNonNegative :: Int
  }
  deriving (Eq, Ord, Show)

-- | Smart constructor for 'NonNegative'.
--
-- Examples:
--
-- >>> mkNonNegative 7
-- Just (MkNonNegative {getNonNegative = 7})
--
-- >>> mkNonNegative (-2)
-- Nothing
mkNonNegative :: Int -> Maybe NonNegative
mkNonNegative n
  | n >= 0 = Just $ MkNonNegative n
  | otherwise = Nothing

-- | Unsafe constructor for 'NonNegative', intended to be used with
-- known constants, e.g., @unsafeNonNegative 7@. Exercise restraint!
unsafeNonNegative :: Int -> NonNegative
unsafeNonNegative n
  | n >= 0 = MkNonNegative n
  | otherwise =
    error $
      "Passed negative "
        <> showt n
        <> " to unsafeNonNegative!"
