-- | Specs for ShellRun.Data.TimeRep.
module Specs.ShellRun.Data.TimeRep (specs) where

import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.TimeRep specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Data.TimeRep"
    [ zero,
      singular,
      pluralMin,
      pluralMinSec,
      hour,
      day
    ]

zero :: TestTree
zero =
  THU.testCase "0 should 0 seconds" $
    "0 seconds" @=? TimeRep.formatTime 0

singular :: TestTree
singular =
  THU.testCase "61 should be singular minute and seconds" $
    "1 minute, 1 second" @=? TimeRep.formatTime 61

pluralMin :: TestTree
pluralMin =
  THU.testCase "180 should be plural minutes" $
    "3 minutes" @=? TimeRep.formatTime 180

pluralMinSec :: TestTree
pluralMinSec =
  THU.testCase "200 should pluralize minutes and seconds" $
    "3 minutes, 20 seconds" @=? TimeRep.formatTime 200

hour :: TestTree
hour =
  THU.testCase "4000 should include hours" $
    "1 hour, 6 minutes, 40 seconds" @=? TimeRep.formatTime 4_000

day :: TestTree
day =
  THU.testCase "100,000 should include days" $
    "1 day, 3 hours, 46 minutes, 40 seconds" @=? TimeRep.formatTime 100_000
