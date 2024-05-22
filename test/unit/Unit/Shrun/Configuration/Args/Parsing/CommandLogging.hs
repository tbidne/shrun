module Unit.Shrun.Configuration.Args.Parsing.CommandLogging (tests) where

import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize (MkReadSize))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CommandLogging"
    [ pollIntervalTests,
      readSizeTests,
      reportReadErrorsTests
    ]

pollIntervalTests :: TestTree
pollIntervalTests =
  testGroup
    "--command-log-poll-interval"
    [ testPollInterval,
      testPollIntervalUnderscores,
      testNoPollInterval
    ]

testPollInterval :: TestTree
testPollInterval =
  testPropertyNamed desc "testPollInterval"
    $ U.verifyResult argList expected
  where
    desc = "Parses --command-log-poll-interval"
    argList = ["--command-log-poll-interval", "1000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #pollInterval) 1000

testPollIntervalUnderscores :: TestTree
testPollIntervalUnderscores =
  testPropertyNamed desc "testPollIntervalUnderscores"
    $ U.verifyResult argList expected
  where
    desc = "Parses --command-log-poll-interval with underscores"
    argList = ["--command-log-poll-interval", "1_000_000", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #pollInterval) 1_000_000

testNoPollInterval :: TestTree
testNoPollInterval =
  testPropertyNamed "Parses --no-command-log-poll-interval" "testNoPollInterval"
    $ U.verifyResult argList expected
  where
    argList = ["--no-command-log-poll-interval", "command"]
    expected = U.disableDefCoreArgs (#commandLogging % #pollInterval)

readSizeTests :: TestTree
readSizeTests =
  testGroup
    "--command-log-read-size"
    [ testReadSize,
      testNoReadSize
    ]

testReadSize :: TestTree
testReadSize =
  testPropertyNamed
    "Parses --command-log-read-size"
    "testReadSize"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-read-size", "2 kb", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #readSize) (MkReadSize $ MkBytes 2_000)

testNoReadSize :: TestTree
testNoReadSize =
  testPropertyNamed "Parses --no-command-log-read-size" "testNoReadSize"
    $ U.verifyResult argList expected
  where
    argList = ["--no-command-log-read-size", "command"]
    expected = U.disableDefCoreArgs (#commandLogging % #readSize)

reportReadErrorsTests :: TestTree
reportReadErrorsTests =
  testGroup
    "--command-log-report-read-errors"
    [ testReportReadErrors,
      testNoReportReadErrors
    ]

testReportReadErrors :: TestTree
testReportReadErrors =
  testPropertyNamed
    "Parses --command-log-report-read-errors"
    "testReportReadErrors"
    $ U.verifyResult argList expected
  where
    argList = ["--command-log-report-read-errors", "command"]
    expected = U.updateDefCoreArgs (#commandLogging % #reportReadErrors) ()

testNoReportReadErrors :: TestTree
testNoReportReadErrors =
  testPropertyNamed
    "Parses --no-command-log-report-read-errors"
    "testNoReportReadErrors"
    $ U.verifyResult argList expected
  where
    argList = ["--no-command-log-report-read-errors", "command"]
    expected = U.disableDefCoreArgs (#commandLogging % #reportReadErrors)
