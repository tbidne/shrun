{-# LANGUAGE OverloadedLists #-}

module Unit.Shrun.Configuration.Args.Parsing.Core (tests) where

import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
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
      testTimeoutDisabled
    ]

testTimeoutShort :: TestTree
testTimeoutShort =
  testPropertyNamed "Parses -t" "testTimeoutShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t7", "command"]
    expected = U.updateDefCoreArgs #timeout (With 7)

testTimeout :: TestTree
testTimeout =
  testPropertyNamed "Parses --timeout" "testTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=7", "command"]
    expected = U.updateDefCoreArgs #timeout (With 7)

testTimeoutUnderscores :: TestTree
testTimeoutUnderscores =
  testPropertyNamed "Parses --timeout with underscores" "testTimeoutUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1_000", "command"]
    expected = U.updateDefCoreArgs #timeout (With 1_000)

testTimeoutStringShort :: TestTree
testTimeoutStringShort =
  testPropertyNamed "Parses -t 2h4s" "testTimeoutStringShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t2h4s", "command"]
    expected = U.updateDefCoreArgs #timeout (With 7204)

testTimeoutString :: TestTree
testTimeoutString =
  testPropertyNamed "Parses --timeout=1d2h3m4s" "testTimeoutString"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1d2h3m4s", "command"]
    expected = U.updateDefCoreArgs #timeout (With 93784)

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

testTimeoutDisabled :: TestTree
testTimeoutDisabled =
  testPropertyNamed "Parses --timeout off" "testTimeoutDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout", "off", "command"]
    expected = U.updateDefCoreArgs #timeout Disabled

initTests :: TestTree
initTests =
  testGroup
    "--init"
    [ testInitShort,
      testInit1,
      testInit2,
      parseInitDisabled
    ]

testInitShort :: TestTree
testInitShort =
  testPropertyNamed "Parses short init" "testInitShort"
    $ U.verifyResult argList expected
  where
    argList = ["-i. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init (With ". ~/.bashrc")

testInit1 :: TestTree
testInit1 =
  testPropertyNamed "Parses --init=. ~/.bashrc" "testInit1"
    $ U.verifyResult argList expected
  where
    argList = ["--init=. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init (With ". ~/.bashrc")

testInit2 :: TestTree
testInit2 =
  testPropertyNamed "Parses --init \". ~/.bashrc\"" "testInit2"
    $ U.verifyResult argList expected
  where
    argList = ["--init", ". ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init (With ". ~/.bashrc")

parseInitDisabled :: TestTree
parseInitDisabled =
  testPropertyNamed "Parses --init off" "parseInitDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--init", "off", "command"]
    expected = U.disableDefCoreArgs #init
