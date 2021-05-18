module ShellRun.Types.NonNegative
  ( NonNegative,
    getNonNegative,
    mkNonNegative,
    unsafeNonNegative,
    prettyPrint,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

newtype NonNegative = MkNonNegative {getNonNegative :: Int}
  deriving (Eq, Ord, Show)

mkNonNegative :: Int -> Maybe NonNegative
mkNonNegative n
  | n >= 0 = Just $ MkNonNegative n
  | otherwise = Nothing

unsafeNonNegative :: Int -> NonNegative
unsafeNonNegative n
  | n >= 0 = MkNonNegative n
  | otherwise =
    error $
      "Passed negative "
        <> show n
        <> " to unsafeNonNegative!"

prettyPrint :: NonNegative -> Text
prettyPrint = T.pack . show . getNonNegative