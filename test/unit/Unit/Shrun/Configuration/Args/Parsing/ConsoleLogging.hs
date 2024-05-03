module Unit.Shrun.Configuration.Args.Parsing.ConsoleLogging (tests) where

import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Data.Truncation (LineTruncation (Detected, Undetected))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.ConsoleLogging"
    [ commandLoggingTests,
      cmdNameTruncTests,
      lineTruncTests,
      stripControlTests
    ]

commandLoggingTests :: TestTree
commandLoggingTests =
  testGroup
    "--console-log-cmd"
    [ testCommandLogging,
      testNoCommandLogging
    ]

testCommandLogging :: TestTree
testCommandLogging =
  testPropertyNamed desc "testCommandLogging"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-cmd"
    argList = ["--console-log-cmd", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #cmdLogging)
        (With ())
        U.defArgs

testNoCommandLogging :: TestTree
testNoCommandLogging =
  testPropertyNamed "Parses --no-console-log-cmd" "testNoCommandLogging"
    $ U.verifyResult argList expected
  where
    argList = ["--no-console-log-cmd", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #cmdLogging)
        Disabled
        U.defArgs

stripControlTests :: TestTree
stripControlTests =
  testGroup
    "--console-log-strip-control"
    [ testStripControlAll,
      testStripControlNone,
      testStripControlSmart,
      testNoStripControl
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

testStripControlNone :: TestTree
testStripControlNone =
  testPropertyNamed desc "testStripControlNone"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-strip-control=None"
    argList = ["--console-log-strip-control=none", "command"]
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

testNoStripControl :: TestTree
testNoStripControl =
  testPropertyNamed desc "testNoStripControl"
    $ U.verifyResult argList expected
  where
    desc = "Parses --no-console-log-strip-control"
    argList = ["--no-console-log-strip-control", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #stripControl)

cmdNameTruncTests :: TestTree
cmdNameTruncTests =
  testGroup
    "--console-log-cmd-name-trunc"
    [ testCmdNameTrunc,
      testNoCmdNameTrunc
    ]

testCmdNameTrunc :: TestTree
testCmdNameTrunc =
  testPropertyNamed
    "Parses --console-log-cmd-name-trunc"
    "testCmdNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-cmd-name-trunc", "15", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #cmdNameTrunc) 15

testNoCmdNameTrunc :: TestTree
testNoCmdNameTrunc =
  testPropertyNamed
    "Parses --no-console-log-cmd-name-trunc"
    "testNoCmdNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--no-console-log-cmd-name-trunc", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #cmdNameTrunc)

lineTruncTests :: TestTree
lineTruncTests =
  testGroup
    "--console-log-line-trunc"
    [ testLineTrunc,
      testLineTruncDetect,
      testNoLineTrunc
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
        (Undetected 15)

testLineTruncDetect :: TestTree
testLineTruncDetect =
  testPropertyNamed desc "testLineTruncDetect"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-line-trunc detect"
    argList = ["--console-log-line-trunc", "detect", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #lineTrunc) Detected

testNoLineTrunc :: TestTree
testNoLineTrunc =
  testPropertyNamed "Parses --no-console-log-line-trunc" "testNoLineTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--no-console-log-line-trunc", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #lineTrunc)
