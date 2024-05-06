module Unit.Shrun.Configuration.Args.Parsing.ConsoleLogging (tests) where

import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation (LineTruncation (Detected, Undetected))
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.ConsoleLogging"
    [ commandLoggingTests,
      commandNameTruncTests,
      lineTruncTests,
      stripControlTests
    ]

commandLoggingTests :: TestTree
commandLoggingTests =
  testGroup
    "--console-log-command"
    [ testCommandLogging,
      testNoCommandLogging
    ]

testCommandLogging :: TestTree
testCommandLogging =
  testPropertyNamed desc "testCommandLogging"
    $ U.verifyResult argList expected
  where
    desc = "Parses --console-log-command"
    argList = ["--console-log-command", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #commandLogging)
        (With ())
        U.defArgs

testNoCommandLogging :: TestTree
testNoCommandLogging =
  testPropertyNamed "Parses --no-console-log-command" "testNoCommandLogging"
    $ U.verifyResult argList expected
  where
    argList = ["--no-console-log-command", "command"]
    expected =
      set'
        (_Just % #coreConfig % #consoleLogging % #commandLogging)
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

commandNameTruncTests :: TestTree
commandNameTruncTests =
  testGroup
    "--console-log-command-name-trunc"
    [ testCommandNameTrunc,
      testNoCommandNameTrunc
    ]

testCommandNameTrunc :: TestTree
testCommandNameTrunc =
  testPropertyNamed
    "Parses --console-log-command-name-trunc"
    "testCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--console-log-command-name-trunc", "15", "command"]
    expected = U.updateDefCoreArgs (#consoleLogging % #commandNameTrunc) 15

testNoCommandNameTrunc :: TestTree
testNoCommandNameTrunc =
  testPropertyNamed
    "Parses --no-console-log-command-name-trunc"
    "testNoCommandNameTrunc"
    $ U.verifyResult argList expected
  where
    argList = ["--no-console-log-command-name-trunc", "command"]
    expected = U.disableDefCoreArgs (#consoleLogging % #commandNameTrunc)

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
