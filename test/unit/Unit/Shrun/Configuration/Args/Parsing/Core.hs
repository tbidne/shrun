module Unit.Shrun.Configuration.Args.Parsing.Core (tests) where

import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.CommandLogging qualified as CommandLogging
import Unit.Shrun.Configuration.Args.Parsing.CommonLogging qualified as CommonLogging
import Unit.Shrun.Configuration.Args.Parsing.ConsoleLogging qualified as ConsoleLogging
import Unit.Shrun.Configuration.Args.Parsing.FileLogging qualified as FileLogging
import Unit.Shrun.Configuration.Args.Parsing.Notify qualified as Notify
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.Core"
    [ initTests,
      timeoutTests,
      CommonLogging.tests,
      CommandLogging.tests,
      ConsoleLogging.tests,
      FileLogging.tests,
      Notify.tests
    ]

timeoutTests :: TestTree
timeoutTests =
  testGroup
    "--timeout"
    [ testTimeoutShort,
      testTimeout,
      testTimeoutUnderscores,
      testTimeoutStringShort,
      testTimeoutString,
      testTimeoutWordFail,
      testTimeoutNegativeFail,
      testNoTimeout
    ]

testTimeoutShort :: TestTree
testTimeoutShort =
  testPropertyNamed "Parses -t" "testTimeoutShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t7", "command"]
    expected = U.updateDefCoreArgs #timeout 7

testTimeout :: TestTree
testTimeout =
  testPropertyNamed "Parses --timeout" "testTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=7", "command"]
    expected = U.updateDefCoreArgs #timeout 7

testTimeoutUnderscores :: TestTree
testTimeoutUnderscores =
  testPropertyNamed "Parses --timeout with underscores" "testTimeoutUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1_000", "command"]
    expected = U.updateDefCoreArgs #timeout 1_000

testTimeoutStringShort :: TestTree
testTimeoutStringShort =
  testPropertyNamed "Parses -t 2h4s" "testTimeoutStringShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t2h4s", "command"]
    expected = U.updateDefCoreArgs #timeout 7204

testTimeoutString :: TestTree
testTimeoutString =
  testPropertyNamed "Parses --timeout=1d2h3m4s" "testTimeoutString"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1d2h3m4s", "command"]
    expected = U.updateDefCoreArgs #timeout 93784

testTimeoutWordFail :: TestTree
testTimeoutWordFail =
  testPropertyNamed "Parses --timeout=cat failure" "testTimeoutWordFail"
    $ U.verifyFailure argList
  where
    argList = ["--timeout=cat", "command"]

testTimeoutNegativeFail :: TestTree
testTimeoutNegativeFail =
  testPropertyNamed "Parses --timeout=cat -7" "testTimeoutNegativeFail"
    $ U.verifyFailure argList
  where
    argList = ["--timeout=-7", "command"]

testNoTimeout :: TestTree
testNoTimeout =
  testPropertyNamed "Parses --no-timeout" "testNoTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--no-timeout", "command"]
    expected = U.disableDefCoreArgs #timeout

initTests :: TestTree
initTests =
  testGroup
    "--init"
    [ testInitShort,
      testInit1,
      testInit2,
      parseNoInit
    ]

testInitShort :: TestTree
testInitShort =
  testPropertyNamed "Parses short init" "testInitShort"
    $ U.verifyResult argList expected
  where
    argList = ["-i. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

testInit1 :: TestTree
testInit1 =
  testPropertyNamed "Parses --init=. ~/.bashrc" "testInit1"
    $ U.verifyResult argList expected
  where
    argList = ["--init=. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

testInit2 :: TestTree
testInit2 =
  testPropertyNamed "Parses --init \". ~/.bashrc\"" "testInit2"
    $ U.verifyResult argList expected
  where
    argList = ["--init", ". ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

parseNoInit :: TestTree
parseNoInit =
  testPropertyNamed "Parses --no-init" "parseNoInit"
    $ U.verifyResult argList expected
  where
    argList = ["--no-init", "command"]
    expected = U.disableDefCoreArgs #init
