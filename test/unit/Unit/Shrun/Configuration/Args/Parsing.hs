{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing (tests) where

import Data.Sequence qualified as Seq
import Shrun.Configuration.Args (defaultArgs)
import Shrun.Utils qualified as U
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.Core qualified as Core
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing"
    [ defaultTests,
      configTests,
      Core.tests,
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
      testNoConfig
    ]

testConfigShort :: TestTree
testConfigShort =
  testPropertyNamed "Parses -c" "testConfigShort"
    $ U.verifyResult argList expected
  where
    argList = ["-c./path/config.toml", "command"]
    expected = U.updateDefArgs #configPath [osp|./path/config.toml|]

testConfig :: TestTree
testConfig =
  testPropertyNamed "Parses --config" "testConfig"
    $ U.verifyResult argList expected
  where
    argList = ["--config=./path/config.toml", "command"]
    expected = U.updateDefArgs #configPath [osp|./path/config.toml|]

testNoConfig :: TestTree
testNoConfig =
  testPropertyNamed "Parses --no-config" "testNoConfig"
    $ U.verifyResult argList expected
  where
    argList = ["--no-config", "command"]
    expected = U.disableDefArgs #configPath

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
    cmds = U.unsafeListToNESeq ["one", "two", "three"]
