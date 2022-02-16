-- | Specs for ShellRun.Commands.
module Specs.ShellRun.Command (specs) where

import Data.HashMap.Strict qualified as Map
import ShellRun.Command (Command (..))
import ShellRun.Command qualified as Command
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Legend (LegendErr (..), LegendMap)
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.Commands specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Command"
    [ translateOneCmd,
      returnsNonMapCmd,
      returnsRecursiveCmds,
      returnsRecursiveAndOtherCmds,
      noSplitNonKeyCmd,
      cycleCmdFail
    ]

translateOneCmd :: TestTree
translateOneCmd = THU.testCase "Should translate one command" $ do
  let result = Command.translateCommands legend (NESeq.singleton "one")
      expected = Right $ NESeq.singleton $ MkCommand (Just "one") "cmd1"
  expected @=? result

returnsNonMapCmd :: TestTree
returnsNonMapCmd = THU.testCase "Should return non-map command" $ do
  let result = Command.translateCommands legend (NESeq.singleton "other")
      expected = Right $ NESeq.singleton $ MkCommand Nothing "other"
  expected @=? result

returnsRecursiveCmds :: TestTree
returnsRecursiveCmds = THU.testCase "Should return recursive commands" $ do
  let result = Command.translateCommands legend (NESeq.singleton "all")
      expected =
        Right $
          MkCommand (Just "one") "cmd1"
            :|^ [ MkCommand (Just "two") "cmd2",
                  MkCommand (Just "all") "cmd3"
                ]
  expected @=? result

returnsRecursiveAndOtherCmds :: TestTree
returnsRecursiveAndOtherCmds = THU.testCase "Should return recursive commands and other" $ do
  let result = Command.translateCommands legend ("all" :|^ ["other"])
      expected =
        Right $
          MkCommand (Just "one") "cmd1"
            :|^ [ MkCommand (Just "two") "cmd2",
                  MkCommand (Just "all") "cmd3",
                  MkCommand Nothing "other"
                ]
  expected @=? result

noSplitNonKeyCmd :: TestTree
noSplitNonKeyCmd = THU.testCase "Should not split non-key commands" $ do
  let result = Command.translateCommands legend (NESeq.singleton "echo ,,")
      expected = Right $ NESeq.singleton $ MkCommand Nothing "echo ,,"
  expected @=? result

cycleCmdFail :: TestTree
cycleCmdFail = THU.testCase "Should fail on cycle" $ do
  let result = Command.translateCommands cyclicLegend (NESeq.singleton "a")
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
