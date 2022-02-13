-- | Specs for ShellRun.Parsing.Commands.
module Specs.ShellRun.Parsing.Commands (specs) where

import Data.HashMap.Strict qualified as Map
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr (..), LegendMap)
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.Parsing.Commands specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Parsing.Commands"
    [ translateOneCmd,
      returnsNonMapCmd,
      returnsRecursiveCmds,
      returnsRecursiveAndOtherCmds,
      noSplitNonKeyCmd,
      cycleCmdFail
    ]

translateOneCmd :: TestTree
translateOneCmd = THU.testCase "Should translate one command" $ do
  let result = ParseCommands.translateCommands legend ["one"]
  Right [MkCommand (Just "one") "cmd1"] @=? result

returnsNonMapCmd :: TestTree
returnsNonMapCmd = THU.testCase "Should return non-map command" $ do
  let result = ParseCommands.translateCommands legend ["other"]
  Right [MkCommand Nothing "other"] @=? result

returnsRecursiveCmds :: TestTree
returnsRecursiveCmds = THU.testCase "Should return recursive commands" $ do
  let result = ParseCommands.translateCommands legend ["all"]
      expected =
        Right
          [ MkCommand (Just "one") "cmd1",
            MkCommand (Just "two") "cmd2",
            MkCommand (Just "all") "cmd3"
          ]
  expected @=? result

returnsRecursiveAndOtherCmds :: TestTree
returnsRecursiveAndOtherCmds = THU.testCase "Should return recursive commands and other" $ do
  let result = ParseCommands.translateCommands legend ["all", "other"]
      expected =
        Right
          [ MkCommand (Just "one") "cmd1",
            MkCommand (Just "two") "cmd2",
            MkCommand (Just "all") "cmd3",
            MkCommand Nothing "other"
          ]
  expected @=? result

noSplitNonKeyCmd :: TestTree
noSplitNonKeyCmd = THU.testCase "Should not split non-key commands" $ do
  let result = ParseCommands.translateCommands legend ["echo ,,"]
  Right [MkCommand Nothing "echo ,,"] @=? result

cycleCmdFail :: TestTree
cycleCmdFail = THU.testCase "Should fail on cycle" $ do
  let result = ParseCommands.translateCommands cyclicLegend ["a"]
  Left (CyclicKeyErr "a -> b -> c -> a") @=? result

legend :: LegendMap
legend =
  Map.fromList
    [ ("one", "cmd1"),
      ("two", "cmd2"),
      ("three", "cmd3"),
      ("oneAndTwo", "one,,two"),
      ("all", "oneAndTwo,,cmd3")
    ]

cyclicLegend :: LegendMap
cyclicLegend =
  Map.fromList
    [ ("a", "b,,x"),
      ("b", "c,,x"),
      ("c", "a,,x")
    ]
