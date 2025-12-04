{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing (tests) where

import Data.Sequence qualified as Seq
import Shrun.Configuration.Args (defaultArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.Core qualified as Core
import Unit.Shrun.Configuration.Args.Parsing.Graph qualified as Graph
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing"
    [ defaultTests,
      configTests,
      Graph.tests,
      Core.tests,
      dryRunTests,
      commandTests
    ]

defaultTests :: TestTree
defaultTests =
  testGroup
    "Defaults"
    [testDefaultArgs]

testDefaultArgs :: TestTree
testDefaultArgs = testPropertyNamed desc "testDefaultArgs" $ do
  let argList = ["command"]
      expected = Just $ defaultArgs ("command" :<|| Seq.empty)
  U.verifyResult argList expected
  where
    desc = "Parses default args"

configTests :: TestTree
configTests =
  testGroup
    "--config"
    [ testConfigShort,
      testConfig,
      testConfigDisabled,
      testConfigMany
    ]

testConfigShort :: TestTree
testConfigShort =
  testPropertyNamed "Parses -c" "testConfigShort"
    $ U.verifyResult argList expected
  where
    argList = ["-c./path/config.toml", "command"]
    expected =
      set'
        (_Just % #configPaths)
        (Seq.singleton (With [osp|./path/config.toml|]))
        U.defArgs

testConfig :: TestTree
testConfig =
  testPropertyNamed "Parses --config" "testConfig"
    $ U.verifyResult argList expected
  where
    argList = ["--config=./path/config.toml", "command"]
    expected =
      set'
        (_Just % #configPaths)
        [With [osp|./path/config.toml|]]
        U.defArgs

testConfigDisabled :: TestTree
testConfigDisabled =
  testPropertyNamed "Parses --config off" "testConfigDisabled"
    $ U.verifyResult argList expected
  where
    argList = ["--config", "off", "command"]
    expected = set' (_Just % #configPaths) [Disabled] U.defArgs

testConfigMany :: TestTree
testConfigMany =
  testPropertyNamed "Parses many --config" "testConfigMany"
    $ U.verifyResult argList expected
  where
    argList =
      [ "-c",
        "./path/t1.toml",
        "-c",
        "off",
        "-c",
        "./path/t2.toml",
        "command"
      ]
    expected =
      set'
        (_Just % #configPaths)
        [With [osp|./path/t1.toml|], Disabled, With [osp|./path/t2.toml|]]
        U.defArgs

commandTests :: TestTree
commandTests =
  testGroup
    "Commands"
    [ emptyCommandsFail,
      testCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail =
  testPropertyNamed "Empty commands fail" "emptyCommandsFail"
    $ U.verifyFailure []

testCommands :: TestTree
testCommands =
  testPropertyNamed "Bare strings parsed as commands" "testCommands"
    $ U.verifyResult argList expected
  where
    argList = ["one", "two", "three"]
    expected = ((_Just % #commands) .~ cmds) U.defArgs
    cmds = unsafeListToNESeq ["one", "two", "three"]

dryRunTests :: TestTree
dryRunTests =
  testGroup
    "--dry-run"
    [ testDryRun
    ]

testDryRun :: TestTree
testDryRun =
  testPropertyNamed "Parses --dry-run" "testDryRun"
    $ U.verifyResult argList expected
  where
    argList = ["--dry-run", "command"]
    expected = set' (_Just % #dryRun) True U.defArgs
