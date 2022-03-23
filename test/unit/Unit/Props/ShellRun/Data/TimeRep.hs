-- | Property tests for ShellRun.Data.TimeRep
module Unit.Props.ShellRun.Data.TimeRep
  ( props,
  )
where

import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Data.TimeRep (TimeRep (..))
import ShellRun.Data.TimeRep qualified as TimeRep
import Test.Tasty qualified as T
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude

-- | Entry point for ShellRun.Data.TimeRep property tests.
props :: TestTree
props = T.testGroup "ShellRun.Data.TimeRep" [fromSeconds]

fromSeconds :: TestTree
fromSeconds = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat "fromSeconds transforms correctly" "fromSeconds" $
    H.withTests limit $
      H.property $ do
        totalSeconds <- H.forAll genTime
        let ts@(MkTimeRep d h m s) = TimeRep.fromSeconds totalSeconds
        H.annotateShow ts
        H.cover
          15
          "1 day <= time"
          (totalSeconds >= TimeRep.secondsInDay)
        H.cover
          10
          "1 hour <= time < 1 day"
          (totalSeconds >= TimeRep.secondsInHour && totalSeconds < TimeRep.secondsInDay)
        H.cover
          5
          "1 minute <= time < 1 hour"
          (totalSeconds >= TimeRep.secondsInMinute && totalSeconds < TimeRep.secondsInHour)
        H.cover 2 "time < 1 minute" (totalSeconds < TimeRep.secondsInMinute)
        -- days = totalSeconds / 86,400
        H.assert $ d == (totalSeconds `div` TimeRep.secondsInDay)
        -- hours = totalSeconds / 3,600 (mod 86,400)
        H.assert $ h == (totalSeconds `mod` TimeRep.secondsInDay `div` TimeRep.secondsInHour)
        -- minutes = totalSeconds / 60 (mod 3,600)
        H.assert $ m == (totalSeconds `mod` TimeRep.secondsInHour `div` TimeRep.secondsInMinute)
        -- seconds = totalSeconds (mod 60)
        H.assert $ s == (totalSeconds `mod` TimeRep.secondsInMinute)

genTime :: Gen Natural
genTime = do
  Gen.frequency
    [ (1, genSeconds),
      (2, genMinutes),
      (3, genHours),
      (4, genDays)
    ]

genSeconds :: Gen Natural
genSeconds = Gen.integral (Range.constantFrom 0 0 (TimeRep.secondsInMinute - 1))

genMinutes :: Gen Natural
genMinutes =
  Gen.integral
    ( Range.constantFrom
        TimeRep.secondsInMinute
        TimeRep.secondsInMinute
        (TimeRep.secondsInHour - 1)
    )

genHours :: Gen Natural
genHours =
  Gen.integral
    ( Range.constantFrom
        TimeRep.secondsInHour
        TimeRep.secondsInHour
        (TimeRep.secondsInDay - 1)
    )

genDays :: Gen Natural
genDays =
  Gen.integral
    ( Range.constantFrom
        TimeRep.secondsInDay
        TimeRep.secondsInDay
        1_000_000
    )
