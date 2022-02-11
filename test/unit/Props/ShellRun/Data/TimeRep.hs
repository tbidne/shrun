-- | Property tests for ShellRun.Data.TimeRep
module Props.ShellRun.Data.TimeRep
  ( props,
  )
where

import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Refined (NonNegative, Refined)
import Refined qualified as R
import Refined.Unsafe qualified as R
import ShellRun.Data.TimeRep (TimeRep (..))
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Data.TimeRep property tests.
props :: TestTree
props = T.testGroup "ShellRun.Data.TimeRep" [fromSeconds]

fromSeconds :: TestTree
fromSeconds = TH.testProperty "fromSeconds transforms correctly" $
  H.property $ do
    totalSeconds <- H.forAll genTime
    let secondsRaw = R.unrefine totalSeconds
        ts@(MkTimeRep d h m s) = TimeRep.fromSeconds totalSeconds
    H.annotateShow ts
    H.cover 15 "1 day <= time" (secondsRaw >= 86_400)
    H.cover 10 "1 hour <= time < 1 day" (secondsRaw >= 3_600 && secondsRaw < 86_400)
    H.cover 5 "1 minute <= time < 1 hour" (secondsRaw >= 60 && secondsRaw < 3_600)
    H.cover 2 "time < 1 minute" (secondsRaw < 60)
    -- days = totalSeconds / 86,400
    H.assert $ R.unrefine d == (secondsRaw `div` 86_400)
    -- hours = totalSeconds / 3,600 (mod 86,400)
    H.assert $ R.unrefine h == (secondsRaw `mod` 86_400 `div` 3_600)
    -- minutes = totalSeconds / 60 (mod 3,600)
    H.assert $ R.unrefine m == (secondsRaw `mod` 3_600 `div` 60)
    -- seconds = totalSeconds (mod 60)
    H.assert $ R.unrefine s == (secondsRaw `mod` 60)

genTime :: Gen (Refined NonNegative Int)
genTime = do
  Gen.frequency
    [ (1, genSeconds),
      (2, genMinutes),
      (3, genHours),
      (4, genDays)
    ]

genSeconds :: Gen (Refined NonNegative Int)
genSeconds = R.unsafeRefine <$> Gen.integral (Range.constantFrom 0 0 59)

genMinutes :: Gen (Refined NonNegative Int)
genMinutes = R.unsafeRefine <$> Gen.integral (Range.constantFrom 60 60 3_599)

genHours :: Gen (Refined NonNegative Int)
genHours = R.unsafeRefine <$> Gen.integral (Range.constantFrom 3_600 3_600 86_399)

genDays :: Gen (Refined NonNegative Int)
genDays = R.unsafeRefine <$> Gen.integral (Range.constantFrom 86_400 86_400 1_000_000)
