-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing.CommonLogging (tests) where

import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOn),
  )
import Shrun.Configuration.Data.CommonLogging.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact,
        ProseFull
      ),
  )
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CommonLogging"
    [ keyHideTests,
      timerFormatTests
    ]

keyHideTests :: TestTree
keyHideTests =
  testGroup
    "--log-key-hide"
    [ testKeyHide,
      testNoKeyHide
    ]

testKeyHide :: TestTree
testKeyHide =
  testPropertyNamed "Parses --log-key-hide" "testKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--log-key-hide", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #keyHide) KeyHideOn

testNoKeyHide :: TestTree
testNoKeyHide =
  testPropertyNamed "Parses --no-log-key-hide" "testNoKeyHide"
    $ U.verifyResult argList expected
  where
    argList = ["--no-log-key-hide", "command"]
    expected = U.disableDefCoreArgs (#commonLogging % #keyHide)

timerFormatTests :: TestTree
timerFormatTests =
  testGroup
    "--log-timer-format"
    [ testTimerFormatDigitalCompact,
      testTimerFormatDigitalFull,
      testTimerFormatProseCompact,
      testTimerFormatProseFull,
      testNoTimerFormat
    ]

testTimerFormatDigitalCompact :: TestTree
testTimerFormatDigitalCompact =
  testPropertyNamed desc "testTimerFormatDigitalCompact"
    $ U.verifyResult argList expected
  where
    desc = "Parses --log-timer-format digital_compact"
    argList = ["--log-timer-format", "digital_compact", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #timerFormat) DigitalCompact

testTimerFormatDigitalFull :: TestTree
testTimerFormatDigitalFull =
  testPropertyNamed
    "Parses --log-timer-format digital_full as DigitalFull"
    "testTimerFormatDigitalFull"
    $ U.verifyResult argList expected
  where
    argList = ["--log-timer-format", "digital_full", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #timerFormat) DigitalFull

testTimerFormatProseCompact :: TestTree
testTimerFormatProseCompact =
  testPropertyNamed
    "Parse --log-timer-format prose_compact"
    "testTimerFormatProseCompact"
    $ U.verifyResult argList expected
  where
    argList = ["--log-timer-format", "prose_compact", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #timerFormat) ProseCompact

testTimerFormatProseFull :: TestTree
testTimerFormatProseFull =
  testPropertyNamed desc "testTimerFormatProseFull"
    $ U.verifyResult argList expected
  where
    desc = "Parses --log-timer-format prose_full"
    argList = ["--log-timer-format", "prose_full", "command"]
    expected = U.updateDefCoreArgs (#commonLogging % #timerFormat) ProseFull

testNoTimerFormat :: TestTree
testNoTimerFormat =
  testPropertyNamed "Parses --no-log-timer-format" "testNoTimerFormat"
    $ U.verifyResult argList expected
  where
    argList = ["--no-log-timer-format", "command"]
    expected = U.disableDefCoreArgs (#commonLogging % #timerFormat)
