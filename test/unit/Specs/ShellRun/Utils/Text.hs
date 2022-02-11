-- | Specs for ShellRun.Utils.Text.
module Specs.ShellRun.Utils.Text (specs) where

import ShellRun.Prelude
import ShellRun.Utils.Text qualified as TextUtils
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Utils.Text specs.
specs :: IO (List TestTree)
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Utils.Text" $ do
    Hspec.describe "breakStripPoint" $ do
      Hspec.it "Missing key should return (str, \"\")" $ do
        TextUtils.breakStripPoint point "ab" `shouldBe` ("ab", "")
      Hspec.it "Normal case should strip out key" $ do
        TextUtils.breakStripPoint point "abc=def" `shouldBe` ("abc", "def")
      Hspec.it "Multiple keys should only break on first)" $ do
        TextUtils.breakStripPoint point "ab=cd=ef" `shouldBe` ("ab", "cd=ef")
      Hspec.it "Leading key should return (\"\", str)" $ do
        TextUtils.breakStripPoint point "=ab" `shouldBe` ("", "ab")
      Hspec.it "Trailing key should return (str, \"\")" $ do
        TextUtils.breakStripPoint point "ab=" `shouldBe` ("ab", "")
  where
    point = TextUtils.unsafeMkNonEmptyText "="
