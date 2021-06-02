{-# LANGUAGE ImportQualifiedPost #-}

-- | Provides the 'NonNegative' type for safe mathematical
-- operations.
module ShellRun.Math.NonNegative
  ( NonNegative,
    getNonNegative,
    mkNonNegative,
    unsafeNonNegative,
    prettyPrint,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Newtype wrapper over 'Int'.
newtype NonNegative = MkNonNegative
  { -- | Unwraps the 'NonNegative'
    getNonNegative :: Int
  }
  deriving (Eq, Ord, Show)

-- | Smart constructor for 'NonNegative'.
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
        <> show n
        <> " to unsafeNonNegative!"

-- | Convenience function for getting the 'Text' representation
-- of the underlying 'Int'.
prettyPrint :: NonNegative -> Text
prettyPrint = T.pack . show . getNonNegative
