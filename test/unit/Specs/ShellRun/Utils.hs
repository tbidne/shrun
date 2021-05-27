{-# LANGUAGE ImportQualifiedPost #-}

module Specs.ShellRun.Utils (specs) where

import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Utils qualified as Utils
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Utils" $ do
    Hspec.describe "formatSeconds" $ do
      Hspec.it "0 should pluralize minutes and seconds" $ do
        Utils.formatSeconds (NN.unsafeNonNegative 0) `shouldBe` "0 minutes and 0 seconds"
      Hspec.it "61 should be singular minute and seconds" $ do
        Utils.formatSeconds (NN.unsafeNonNegative 61) `shouldBe` "1 minute and 1 second"
      Hspec.it "180 should be plural minutes and singular seconds" $ do
        Utils.formatSeconds (NN.unsafeNonNegative 180) `shouldBe` "3 minutes and 0 seconds"
      Hspec.it "200 should pluralize minutes and seconds" $ do
        Utils.formatSeconds (NN.unsafeNonNegative 200) `shouldBe` "3 minutes and 20 seconds"
