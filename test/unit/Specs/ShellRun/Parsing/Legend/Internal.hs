-- | Specs for ShellRun.Parsing.Legend.Internal.
module Specs.ShellRun.Parsing.Legend.Internal (specs) where

import Data.HashMap.Strict qualified as Map
import ShellRun.Data.Legend (LegendErr (..))
import ShellRun.Parsing.Legend.Internal qualified as Internal
import ShellRun.Prelude
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Parsing.Legend.Internal specs.
specs :: IO (List TestTree)
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Parsing.Legend.Internal" $ do
    Hspec.it "Should parse to map and skip comments" $ do
      Internal.linesToMap ["a=b,,k", "b=c", "#c=x"]
        `shouldBe` Right
          ( Map.fromList
              [ ("a", "b,,k"),
                ("b", "c")
              ]
          )
    Hspec.it "Empty key should throw error" $ do
      Internal.linesToMap ["=b"] `shouldBe` Left (EntryErr "Key cannot be empty: =b")
    Hspec.it "Empty value should throw error" $ do
      Internal.linesToMap ["a="] `shouldBe` Left (EntryErr "Value cannot be empty: a=")
    Hspec.it "Duplicate keys should throw error" $ do
      Internal.linesToMap ["a=b", "b=c", "a=d"] `shouldBe` Left (DuplicateKeyErr "a")
