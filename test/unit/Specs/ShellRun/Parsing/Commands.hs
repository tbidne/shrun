{-# LANGUAGE ImportQualifiedPost #-}

module Specs.ShellRun.Parsing.Commands (specs) where

import Data.Map.Strict qualified as Map
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendMap)
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Parsing.Commands" $ do
    Hspec.it "Should translate one command" $ do
      ParseCommands.translateCommands legend ["one"]
        `shouldBe` fmap MkCommand ["cmd1"]
    Hspec.it "Should return non-map command" $ do
      ParseCommands.translateCommands legend ["other"]
        `shouldBe` fmap MkCommand ["other"]
    Hspec.it "Should return recursive commands" $ do
      ParseCommands.translateCommands legend ["all"]
        `shouldBe` fmap MkCommand ["cmd1", "cmd2", "cmd3"]
    Hspec.it "Should return recursive commands and other" $ do
      ParseCommands.translateCommands legend ["all", "other"]
        `shouldBe` fmap MkCommand ["cmd1", "cmd2", "cmd3", "other"]
    Hspec.it "Should not split non-key commands" $ do
      ParseCommands.translateCommands legend ["echo ,,"]
        `shouldBe` fmap MkCommand ["echo ,,"]

legend :: LegendMap
legend =
  Map.fromList
    [ ("one", "cmd1"),
      ("two", "cmd2"),
      ("three", "cmd3"),
      ("oneAndTwo", "one,,two"),
      ("all", "oneAndTwo,,cmd3")
    ]