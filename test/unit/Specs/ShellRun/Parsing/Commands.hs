{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specs for ShellRun.Parsing.Commands.
module Specs.ShellRun.Parsing.Commands (specs) where

import Data.Map.Strict qualified as Map
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr (..), LegendMap)
import ShellRun.Parsing.Commands qualified as ParseCommands
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Parsing.Commands specs.
specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Parsing.Commands" $ do
    Hspec.it "Should translate one command" $ do
      ParseCommands.translateCommands legend ["one"]
        `shouldBe` Right [MkCommand (Just "one") "cmd1"]
    Hspec.it "Should return non-map command" $ do
      ParseCommands.translateCommands legend ["other"]
        `shouldBe` Right [MkCommand Nothing "other"]
    Hspec.it "Should return recursive commands" $ do
      ParseCommands.translateCommands legend ["all"]
        `shouldBe` Right
          [ MkCommand (Just "one") "cmd1",
            MkCommand (Just "two") "cmd2",
            MkCommand (Just "all") "cmd3"
          ]
    Hspec.it "Should return recursive commands and other" $ do
      ParseCommands.translateCommands legend ["all", "other"]
        `shouldBe` Right
          [ MkCommand (Just "one") "cmd1",
            MkCommand (Just "two") "cmd2",
            MkCommand (Just "all") "cmd3",
            MkCommand Nothing "other"
          ]
    Hspec.it "Should not split non-key commands" $ do
      ParseCommands.translateCommands legend ["echo ,,"]
        `shouldBe` Right [MkCommand Nothing "echo ,,"]
    Hspec.it "Should fail on cycle" $ do
      ParseCommands.translateCommands cyclicLegend ["a"]
        `shouldBe` Left (CyclicKeyErr "a -> b -> c -> a")

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
