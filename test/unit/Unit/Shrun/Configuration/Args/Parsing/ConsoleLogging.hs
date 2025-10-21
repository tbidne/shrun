module Unit.Shrun.Configuration.Args.Parsing.ConsoleLogging (tests) where

import Shrun.Configuration.Data.ConsoleLogging (ConsoleLogCmdSwitch (MkConsoleLogCmdSwitch))
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact,
        ProseFull
      ),
  )
import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation
  ( LineTruncation
      ( Detected,
        Undetected
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.ConsoleLogging"
    [ commandLoggingTests,
      commandNameTruncTests,
      lineTruncTests,
      stripControlTests,
      timerFormatTests
    ]

commandLoggingTests :: TestTree
commandLoggingTests =
  testGroup
    "--console-log-command"
    [ testCommandLogging,
      testCommandLoggingFalse
    ]

testCommandLogging :: TestTree
testCommandLogging =
  testPropertyNamed desc "testCommandLogging"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-command on"
    argList = ["--console-log-command", "on", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #commandLogging)
        (Just $ MkConsoleLogCmdSwitch True)
        U.defArgs

testCommandLoggingFalse :: TestTree
testCommandLoggingFalse =
  testPropertyNamed "Parses --console-log-command on" "testNoCommandLogging"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-command", "off", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #commandLogging)
        (Just $ MkConsoleLogCmdSwitch False)
        U.defArgs

stripControlTests :: TestTree
stripControlTests =
  testGroup
    "--console-log-strip-control"
    [ testStripControlAll,
      testStripControlDisabled,
      testStripControlSmart
    ]

testStripControlAll :: TestTree
testStripControlAll =
  testPropertyNamed desc "parseLongStripControlAll"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-strip-control=all"
    argList = ["--console-log-strip-control=all", "command"]
    expected =
      U.updateDefCoreArgs
        (#consoleLogging % #stripControl)
        StripControlAll

testStripControlDisabled :: TestTree
testStripControlDisabled =
  testPropertyNamed desc "testStripControlDisabled"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-strip-control=off"
    argList = ["--console-log-strip-control=off", "command"]
    expected =
      U.updateDefCoreArgs
        (#consoleLogging % #stripControl)
        StripControlNone

testStripControlSmart :: TestTree
testStripControlSmart =
  testPropertyNamed desc "testStripControlSmart"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-strip-control=smart"
    argList = ["--console-log-strip-control=smart", "command"]
    expected =
      U.updateDefCoreArgs
        (#consoleLogging % #stripControl)
        StripControlSmart

commandNameTruncTests :: TestTree
commandNameTruncTests =
  testGroup
    "--console-log-command-name-trunc"
    [ testCommandNameTrunc,
      testCommandNameTruncUnderscores,
      testCommandNameTruncDisabled
    ]

testCommandNameTrunc :: TestTree
testCommandNameTrunc =
  testPropertyNamed
    "Parses --console-log-command-name-trunc"
    "testCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-command-name-trunc", "15", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #commandNameTrunc) (With 15)

testCommandNameTruncUnderscores :: TestTree
testCommandNameTruncUnderscores =
  testPropertyNamed
    "Parses --console-log-command-name-trunc with underscores"
    "testCommandNameTruncUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-command-name-trunc", "12_500", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #commandNameTrunc) (With 12_500)

testCommandNameTruncDisabled :: TestTree
testCommandNameTruncDisabled =
  testPropertyNamed
    "Parses --console-log-command-name-trunc off"
    "testCommandNameTruncDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-command-name-trunc", "off", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #commandNameTrunc)

lineTruncTests :: TestTree
lineTruncTests =
  testGroup
    "--console-log-line-trunc"
    [ testLineTrunc,
      testLineTruncUnderscores,
      testLineTruncDetect,
      testLineTruncDisabled
    ]

testLineTrunc :: TestTree
testLineTrunc =
  testPropertyNamed
    "Parses --console-log-line-trunc"
    "testLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-line-trunc", "15", "command"]
    expected =
      U.updateDefCoreArgs
        (#consoleLogging % #lineTrunc)
        (With $ Undetected 15)

testLineTruncUnderscores :: TestTree
testLineTruncUnderscores =
  testPropertyNamed
    "Parses --console-log-line-trunc with underscores"
    "testLineTruncUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-line-trunc", "1_50", "command"]
    expected =
      U.updateDefCoreArgs
        (#consoleLogging % #lineTrunc)
        (With $ Undetected 150)

testLineTruncDetect :: TestTree
testLineTruncDetect =
  testPropertyNamed desc "testLineTruncDetect"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-line-trunc detect"
    argList = ["--console-log-line-trunc", "detect", "command"]
    expected =
      U.updateDefCoreArgs (#consoleLogging % #lineTrunc) (With Detected)

testLineTruncDisabled :: TestTree
testLineTruncDisabled =
  testPropertyNamed "Parses --console-log-line-trunc off" "testNoLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-line-trunc", "off", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #lineTrunc)

timerFormatTests :: TestTree
timerFormatTests =
  testGroup
    "--console-log-timer-format"
    [ testTimerFormatDigitalCompact,
      testTimerFormatDigitalFull,
      testTimerFormatProseCompact,
      testTimerFormatProseFull
    ]

testTimerFormatDigitalCompact :: TestTree
testTimerFormatDigitalCompact =
  testPropertyNamed desc "testTimerFormatDigitalCompact"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-timer-format digital_compact"
    argList = ["--console-log-timer-format", "digital_compact", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #timerFormat) DigitalCompact

testTimerFormatDigitalFull :: TestTree
testTimerFormatDigitalFull =
  testPropertyNamed
    "Parses --console-log-timer-format digital_full as DigitalFull"
    "testTimerFormatDigitalFull"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-timer-format", "digital_full", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #timerFormat) DigitalFull

testTimerFormatProseCompact :: TestTree
testTimerFormatProseCompact =
  testPropertyNamed
    "Parse --console-log-timer-format prose_compact"
    "testTimerFormatProseCompact"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-timer-format", "prose_compact", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #timerFormat) ProseCompact

testTimerFormatProseFull :: TestTree
testTimerFormatProseFull =
  testPropertyNamed desc "testTimerFormatProseFull"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-timer-format prose_full"
    argList = ["--console-log-timer-format", "prose_full", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #timerFormat) ProseFull
