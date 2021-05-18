module ShellRunner.Types.Positive
  ( Positive,
    getPositive,
    mkPositive,
    unsafePositive,
  )
where

newtype Positive = MkPositive {getPositive :: Int}
  deriving (Eq, Ord, Show)

mkPositive :: Int -> Maybe Positive
mkPositive n
  | n > 0 = Just $ MkPositive n
  | otherwise = Nothing

unsafePositive :: Int -> Positive
unsafePositive n
  | n > 0 = MkPositive n
  | otherwise =
    error $
      "Passed non-positive "
        <> show n
        <> " to unsafePositive!"