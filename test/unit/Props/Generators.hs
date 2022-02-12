-- | Provides various generators for property tests.
module Props.Generators
  ( genNonNegative,
    genPositive,
    genTimeSpec,
    genInt,
    genText,
    getNonEmptyText,
  )
where

import Data.Int (Int64)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Refined (NonNegative, Positive, Refined)
import Refined qualified as R
import Refined.Unsafe qualified as R
import ShellRun.Prelude
import System.Clock (TimeSpec (..))

-- | Generates 'NonNegative' in [0, 1_000_000].
genNonNegative :: Gen (Refined NonNegative Int)
genNonNegative = R.unsafeRefine <$> Gen.integral (Range.constant 0 1_000_000)

-- | Generates 'Positive' in [1, 1_000_000].
genPositive :: Gen (Refined Positive Int)
genPositive = R.unsafeRefine <$> Gen.integral (Range.constant 1 1_000_000)

-- | Generates 'TimeSpec' where 'sec' and 'nsec' are random 'Int64'.
genTimeSpec :: Gen TimeSpec
genTimeSpec = do
  TimeSpec
    <$> genInt64
    <*> genInt64

genInt64 :: Gen Int64
genInt64 =
  let range = Range.linearFrom 0 minBound maxBound
   in Gen.int64 range

-- | Generates 'Int' based on 'Bounded'.
genInt :: Gen Int
genInt =
  let range = Range.linearFrom 0 minBound maxBound
   in Gen.int range

-- | Generates latin1 'Text' with 0-30 characters.
genText :: Gen Text
genText = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 0 0 30

-- | Generates latin1 'NonEmptyText' with 1-30 characters.
getNonEmptyText :: Gen (Refined R.NonEmpty Text)
getNonEmptyText = R.unsafeRefine <$> Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 30
