-- | Specs for ShellRun.Parsing.Legend.Internal.
module Specs.ShellRun.Parsing.Legend.Internal (specs) where

import Data.HashMap.Strict qualified as Map
import ShellRun.Data.Legend (LegendErr (..))
import ShellRun.Parsing.Legend.Internal qualified as Internal
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for ShellRun.Parsing.Legend.Internal specs.
specs :: TestTree
specs =
  Tasty.testGroup
    "ShellRun.Parsing.Legend.Internal"
    [ parseMapAndSkip,
      emptyKeyThrowErr,
      emptyValThrowErr,
      duplicateKeysThrowErr
    ]

parseMapAndSkip :: TestTree
parseMapAndSkip = THU.testCase "Should parse to map and skip comments" $ do
  let result = Internal.linesToMap ["a=b,,k", "b=c", "#c=x"]
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
  THU.testCase "Empty key should throw error" $
    Left (EntryErr "Key cannot be empty: =b") @=? Internal.linesToMap ["=b"]

emptyValThrowErr :: TestTree
emptyValThrowErr =
  THU.testCase "Empty value should throw error" $
    Left (EntryErr "Value cannot be empty: a=") @=? Internal.linesToMap ["a="]

duplicateKeysThrowErr :: TestTree
duplicateKeysThrowErr =
  THU.testCase "Duplicate keys should throw error" $
    Left (DuplicateKeyErr "a") @=? Internal.linesToMap ["a=b", "b=c", "a=d"]
