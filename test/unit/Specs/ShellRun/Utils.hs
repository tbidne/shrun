-- | Specs for ShellRun.Utils.
module Specs.ShellRun.Utils (specs) where

import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Math qualified as Math
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Utils specs.
specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Utils" $ do
    Hspec.describe "formatTime" $ do
      Hspec.it "0 should 0 seconds" $ do
        Utils.formatTime (Math.unsafeNonNegative 0) `shouldBe` "0 seconds"
      Hspec.it "61 should be singular minute and seconds" $ do
        Utils.formatTime (Math.unsafeNonNegative 61) `shouldBe` "1 minute, 1 second"
      Hspec.it "180 should be plural minutes" $ do
        Utils.formatTime (Math.unsafeNonNegative 180) `shouldBe` "3 minutes"
      Hspec.it "200 should pluralize minutes and seconds" $ do
        Utils.formatTime (Math.unsafeNonNegative 200) `shouldBe` "3 minutes, 20 seconds"
      Hspec.it "4000 should include hours" $ do
        Utils.formatTime (Math.unsafeNonNegative 4_000) `shouldBe` "1 hour, 6 minutes, 40 seconds"
      Hspec.it "100,000 should include days" $ do
        Utils.formatTime (Math.unsafeNonNegative 100_000) `shouldBe` "1 day, 3 hours, 46 minutes, 40 seconds"
    Hspec.describe "displayCommand" $ do
      Hspec.it "should use command when no key exists" $ do
        Utils.displayCommand ShowCommand (MkCommand Nothing "cmd") `shouldBe` "cmd"
        Utils.displayCommand ShowKey (MkCommand Nothing "cmd") `shouldBe` "cmd"
      Hspec.it "should use command with ShowCommand" $ do
        Utils.displayCommand ShowCommand (MkCommand (Just "key") "cmd") `shouldBe` "cmd"
      Hspec.it "should use key with ShowKey when one exists" $ do
        Utils.displayCommand ShowKey (MkCommand (Just "key") "cmd") `shouldBe` "key"
