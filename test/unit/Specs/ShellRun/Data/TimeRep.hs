{-# LANGUAGE TemplateHaskell #-}

-- | Specs for ShellRun.Data.TimeRep.
module Specs.ShellRun.Data.TimeRep (specs) where

import Refined qualified as R
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Prelude
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.TimeRep specs.
specs :: IO [TestTree]
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Data.TimeRep" $ do
    Hspec.describe "formatTime" $ do
      Hspec.it "0 should 0 seconds" $ do
        TimeRep.formatTime $$(R.refineTH 0) `shouldBe` "0 seconds"
      Hspec.it "61 should be singular minute and seconds" $ do
        TimeRep.formatTime $$(R.refineTH 61) `shouldBe` "1 minute, 1 second"
      Hspec.it "180 should be plural minutes" $ do
        TimeRep.formatTime $$(R.refineTH 180) `shouldBe` "3 minutes"
      Hspec.it "200 should pluralize minutes and seconds" $ do
        TimeRep.formatTime $$(R.refineTH 200) `shouldBe` "3 minutes, 20 seconds"
      Hspec.it "4000 should include hours" $ do
        TimeRep.formatTime $$(R.refineTH 4_000) `shouldBe` "1 hour, 6 minutes, 40 seconds"
      Hspec.it "100,000 should include days" $ do
        TimeRep.formatTime $$(R.refineTH 100_000) `shouldBe` "1 day, 3 hours, 46 minutes, 40 seconds"
