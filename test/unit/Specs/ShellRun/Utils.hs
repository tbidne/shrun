-- | Specs for ShellRun.Utils.
module Specs.ShellRun.Utils (specs) where

import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
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
    Hspec.describe "displayCommand" $ do
      Hspec.it "should use command when no key exists" $ do
        Utils.displayCommand ShowCommand (MkCommand Nothing "cmd") `shouldBe` "cmd"
        Utils.displayCommand ShowKey (MkCommand Nothing "cmd") `shouldBe` "cmd"
      Hspec.it "should use command with ShowCommand" $ do
        Utils.displayCommand ShowCommand (MkCommand (Just "key") "cmd") `shouldBe` "cmd"
      Hspec.it "should use key with ShowKey when one exists" $ do
        Utils.displayCommand ShowKey (MkCommand (Just "key") "cmd") `shouldBe` "key"
