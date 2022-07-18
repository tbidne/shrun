{-# LANGUAGE OverloadedLists #-}

-- | Specs for ShellRun.Data.Commands.
module Unit.Specs.ShellRun.Configuration.Legend (specs) where

import Data.HashMap.Strict qualified as Map
import ShellRun.Configuration.Legend
  ( CyclicKeyError (..),
    LegendError (..),
    LegendMap,
    linesToMap,
    translateCommands,
  )
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import Unit.Prelude

-- | Entry point for ShellRun.Data.Commands specs.
specs :: TestTree
specs =
  testGroup
    "ShellRun.Configuration.Legend"
    [ translateSpecs,
      linesToMapSpecs
    ]

translateSpecs :: TestTree
translateSpecs =
  testGroup
    "translateCommands"
    [ translateOneCmd,
      returnsNonMapCmd,
      returnsRecursiveCmds,
      returnsRecursiveAndOtherCmds,
      noSplitNonKeyCmd,
      cycleCmdFail
    ]

translateOneCmd :: TestTree
translateOneCmd = testCase "Should translate one command" $ do
  let result = translateCommands legend (NESeq.singleton "one")
      expected = Right $ NESeq.singleton $ MkCommand (Just "one") "cmd1"
  expected @=? result

returnsNonMapCmd :: TestTree
returnsNonMapCmd = testCase "Should return non-map command" $ do
  let result = translateCommands legend (NESeq.singleton "other")
      expected = Right $ NESeq.singleton $ MkCommand Nothing "other"
  expected @=? result

returnsRecursiveCmds :: TestTree
returnsRecursiveCmds = testCase "Should return recursive commands" $ do
  let result = translateCommands legend (NESeq.singleton "all")
      expected =
        Right $
          MkCommand (Just "one") "cmd1"
            :|^ [ MkCommand (Just "two") "cmd2",
                  MkCommand (Just "all") "cmd3"
                ]
  expected @=? result

returnsRecursiveAndOtherCmds :: TestTree
returnsRecursiveAndOtherCmds = testCase "Should return recursive commands and other" $ do
  let result = translateCommands legend ("all" :|^ ["other"])
      expected =
        Right $
          MkCommand (Just "one") "cmd1"
            :|^ [ MkCommand (Just "two") "cmd2",
                  MkCommand (Just "all") "cmd3",
                  MkCommand Nothing "other"
                ]
  expected @=? result

noSplitNonKeyCmd :: TestTree
noSplitNonKeyCmd = testCase "Should not split non-key commands" $ do
  let result = translateCommands legend (NESeq.singleton "echo ,,")
      expected = Right $ NESeq.singleton $ MkCommand Nothing "echo ,,"
  expected @=? result

cycleCmdFail :: TestTree
cycleCmdFail = testCase "Should fail on cycle" $ do
  let result = translateCommands cyclicLegend (NESeq.singleton "a")
  Left (MkCyclicKeyError "a -> b -> c -> a") @=? result

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

linesToMapSpecs :: TestTree
linesToMapSpecs =
  testGroup
    "linesToMap"
    [ parseMapAndSkip,
      emptyKeyThrowErr,
      emptyValThrowErr,
      duplicateKeysThrowErr
    ]

parseMapAndSkip :: TestTree
parseMapAndSkip = testCase "Should parse to map and skip comments" $ do
  let result = linesToMap ["a=b,,k", "b=c", "#c=x"]
      expected =
        Right
          ( Map.fromList
              [ ("a", "b,,k"),
                ("b", "c")
              ]
          )
  expected @=? result

emptyKeyThrowErr :: TestTree
emptyKeyThrowErr =
  testCase "Empty key should throw error" $
    Left (LegendErrorEmptyKey "=b") @=? linesToMap ["=b"]

emptyValThrowErr :: TestTree
emptyValThrowErr =
  testCase "Empty value should throw error" $
    Left (LegendErrorEmptyValue "a=") @=? linesToMap ["a="]

duplicateKeysThrowErr :: TestTree
duplicateKeysThrowErr =
  testCase "Duplicate keys should throw error" $
    Left (LegendErrorDuplicateKeys "a") @=? linesToMap ["a=b", "b=c", "a=d"]
