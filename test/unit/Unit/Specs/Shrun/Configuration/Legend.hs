{-# LANGUAGE OverloadedLists #-}

-- | Specs for Shrun.Data.Commands.
module Unit.Specs.Shrun.Configuration.Legend (specs) where

import Data.HashMap.Strict qualified as Map
import Shrun.Configuration.Legend
  ( CyclicKeyError (..),
    DuplicateKeyError (..),
    LegendMap,
    linesToMap,
    translateCommands,
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.Legend (unsafeKeyVal)
import Shrun.Data.NonEmptySeq (NonEmptySeq (..), singleton)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Unit.Prelude

-- | Entry point for Shrun.Data.Commands specs.
specs :: TestTree
specs =
  testGroup
    "Shrun.Configuration.Legend"
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
    [ ("one", singleton "cmd1"),
      ("two", singleton "cmd2"),
      ("three", singleton "cmd3"),
      ("oneAndTwo", ["one", "two"]),
      ("all", ["oneAndTwo", "cmd3"])
    ]

cyclicLegend :: LegendMap
cyclicLegend =
  Map.fromList
    [ ("a", ["b", "x"]),
      ("b", ["c", "x"]),
      ("c", ["a", "x"])
    ]

linesToMapSpecs :: TestTree
linesToMapSpecs =
  testGroup
    "linesToMap"
    [ parseMapAndSkip,
      duplicateKeysThrowErr
    ]

parseMapAndSkip :: TestTree
parseMapAndSkip = testCase "Should parse to map and skip comments" $ do
  let result =
        linesToMap
          [ unsafeKeyVal "a" ["b", "k"],
            unsafeKeyVal "b" ["c"]
          ]
      expected =
        Right
          ( Map.fromList
              [ ("a", ["b", "k"]),
                ("b", singleton "c")
              ]
          )
  expected @=? result

duplicateKeysThrowErr :: TestTree
duplicateKeysThrowErr =
  testCase "Duplicate keys should throw error" $
    Left (MkDuplicateKeyError "a") @=? linesToMap result
  where
    result =
      [ unsafeKeyVal "a" ["b"],
        unsafeKeyVal "b" ["c"],
        unsafeKeyVal "a" ["d"]
      ]
