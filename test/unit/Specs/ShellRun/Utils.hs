-- | Specs for ShellRun.Utils.
module Specs.ShellRun.Utils (specs) where

import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Data.TH qualified as TH
import ShellRun.Prelude
import ShellRun.Utils qualified as Utils
import Test.Hspec (shouldBe)
import Test.Hspec qualified as Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec qualified as TH

-- | Entry point for ShellRun.Utils specs.
specs :: IO (List TestTree)
specs = TH.testSpecs $ do
  Hspec.describe "ShellRun.Utils" $ do
    Hspec.describe "displayCommand" $ do
      Hspec.it "should use command when no key exists" $ do
        Utils.displayCommand ShowCommand (MkCommand Nothing "cmd") `shouldBe` "cmd"
        Utils.displayCommand ShowKey (MkCommand Nothing "cmd") `shouldBe` "cmd"
      Hspec.it "should use command with ShowCommand" $ do
        Utils.displayCommand ShowCommand (MkCommand (Just "key") "cmd") `shouldBe` "cmd"
      Hspec.it "should use key with ShowKey when one exists" $ do
        Utils.displayCommand ShowKey (MkCommand (Just "key") "cmd") `shouldBe` "key"
  Hspec.describe "ShellRun.Utils.Text" $ do
    Hspec.describe "breakStripPoint" $ do
      Hspec.it "Missing key should return (str, \"\")" $ do
        Utils.breakStripPoint point "ab" `shouldBe` ("ab", "")
      Hspec.it "Normal case should strip out key" $ do
        Utils.breakStripPoint point "abc=def" `shouldBe` ("abc", "def")
      Hspec.it "Multiple keys should only break on first)" $ do
        Utils.breakStripPoint point "ab=cd=ef" `shouldBe` ("ab", "cd=ef")
      Hspec.it "Leading key should return (\"\", str)" $ do
        Utils.breakStripPoint point "=ab" `shouldBe` ("", "ab")
      Hspec.it "Trailing key should return (str, \"\")" $ do
        Utils.breakStripPoint point "ab=" `shouldBe` ("ab", "")
  where
    point = TH.equalsNE
