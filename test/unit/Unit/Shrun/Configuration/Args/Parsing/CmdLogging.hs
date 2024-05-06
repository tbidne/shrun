module Unit.Shrun.Configuration.Args.Parsing.CmdLogging (tests) where

import Shrun.Data.CmdLogReadSize (CmdLogReadSize (MkCmdLogReadSize))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.CmdLogging"
    [ pollIntervalTests,
      readSizeTests
    ]

readSizeTests :: TestTree
readSizeTests =
  testGroup
    "--cmd-log-read-size"
    [ testReadSize,
      testNoReadSize
    ]

pollIntervalTests :: TestTree
pollIntervalTests =
  testGroup
    "--cmd-log-poll-interval"
    [ testPollInterval,
      testNoPollInterval
    ]

testPollInterval :: TestTree
testPollInterval =
  testPropertyNamed desc "testPollInterval"
    $ U.verifyResult argList expected
  where
    desc = "Parses --cmd-log-poll-interval"
    argList = ["--cmd-log-poll-interval", "1000", "command"]
    expected = U.updateDefCoreArgs (#cmdLogging % #pollInterval) 1000

testNoPollInterval :: TestTree
testNoPollInterval =
  testPropertyNamed "Parses --no-cmd-log-poll-interval" "testNoPollInterval"
    $ U.verifyResult argList expected
  where
    argList = ["--no-cmd-log-poll-interval", "command"]
    expected = U.disableDefCoreArgs (#cmdLogging % #pollInterval)

testReadSize :: TestTree
testReadSize =
  testPropertyNamed
    "Parses --cmd-log-read-size"
    "testReadSize"
    $ U.verifyResult argList expected
  where
    argList = ["--cmd-log-read-size", "2048", "command"]
    expected = U.updateDefCoreArgs (#cmdLogging % #readSize) (MkCmdLogReadSize $ MkBytes 2048)

testNoReadSize :: TestTree
testNoReadSize =
  testPropertyNamed "Parses --no-cmd-log-read-size" "testNoReadSize"
    $ U.verifyResult argList expected
  where
    argList = ["--no-cmd-log-read-size", "command"]
    expected = U.disableDefCoreArgs (#cmdLogging % #readSize)
