{-# LANGUAGE ImportQualifiedPost #-}

module Props.ShellRun.Utils.Internal
  ( props,
  )
where

import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Math (NonNegative, REquals (..))
import ShellRun.Math qualified as Math
import ShellRun.Utils.Internal (TimeSummary (..))
import ShellRun.Utils.Internal qualified as UtilsI
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

props :: TestTree
props = T.testGroup "ShellRun.Utils.Internal" [secondsToTimeSummary]

secondsToTimeSummary :: TestTree
secondsToTimeSummary = TH.testProperty "secondsToTimeSummary transforms correctly" $
  H.property $ do
    seconds <- H.forAll genTime
    let secondsRaw = Math.getNonNegative seconds
        ts@(MkTimeSummary d h m s) = UtilsI.secondsToTimeSummary seconds
    H.annotateShow ts
    H.cover 30 "1 day >= time" (secondsRaw >= 86_400)
    H.cover 20 "1 hour <= time < 1 day" (secondsRaw >= 3_600 && secondsRaw < 86_400)
    H.cover 10 "1 minute <= time < 1 hour" (secondsRaw >= 60 && secondsRaw < 3_600)
    H.cover 2 "time < 1 minute" (secondsRaw < 60)
    -- days = totalSeconds / 86,400
    H.assert $ d =:= (secondsRaw `div` 86_400)
    -- hours = totalSeconds / 3,600 (mod 86,400)
    H.assert $ h =:= (secondsRaw `mod` 86_400 `div` 3_600)
    -- minutes = totalSeconds / 60 (mod 3,600)
    H.assert $ m =:= (secondsRaw `mod` 3_600 `div` 60)
    -- seconds = totalSeconds (mod 60)
    H.assert $ s =:= (secondsRaw `mod` 60)

genTime :: Gen NonNegative
genTime = do
  Gen.frequency
    [ (1, genSeconds),
      (2, genMinutes),
      (3, genHours),
      (4, genDays)
    ]

genSeconds :: Gen NonNegative
genSeconds = Math.unsafeNonNegative <$> Gen.integral (Range.constantFrom 0 0 59)

genMinutes :: Gen NonNegative
genMinutes = Math.unsafeNonNegative <$> Gen.integral (Range.constantFrom 60 60 3_599)

genHours :: Gen NonNegative
genHours = Math.unsafeNonNegative <$> Gen.integral (Range.constantFrom 3_600 3_600 86_399)

genDays :: Gen NonNegative
genDays = Math.unsafeNonNegative <$> Gen.integral (Range.constantFrom 86_400 86_400 1_000_000)
