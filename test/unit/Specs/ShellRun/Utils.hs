module Specs.ShellRun.Utils (specs) where

import ShellRun.Types.NonNegative as NN
import ShellRun.Utils qualified as Utils
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Utils" $ do
    Hspec.it "formatSeconds" $ do
      Utils.formatSeconds (NN.unsafeNonNegative 0) `shouldBe` "0 minutes and 0 seconds"
      Utils.formatSeconds (NN.unsafeNonNegative 61) `shouldBe` "1 minute and 1 second"
      Utils.formatSeconds (NN.unsafeNonNegative 180) `shouldBe` "3 minutes and 0 seconds"
      Utils.formatSeconds (NN.unsafeNonNegative 200) `shouldBe` "3 minutes and 20 seconds"