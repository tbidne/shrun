{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import MockShell.BadLegendMockShell (BadLegendMockShell (..))
import MockShell.GoodMockShell (GoodMockShell (..))
import MockShell.MockShellBase (MockShellBase (..))
import MockShell.NoLegendMockShell (NoLegendMockShell (..))
import ShellRun qualified
import Test.Hspec (Spec, shouldSatisfy)
import Test.Hspec qualified as Hspec
import Test.Tasty qualified as T
import Test.Tasty.Hspec qualified as TH

main :: IO ()
main = tastySpec >>= T.defaultMain
  where
    tastySpec = T.testGroup "Integration Tests" <$> TH.testSpecs spec

spec :: Spec
spec =
  Hspec.describe "Run mock shells" $ do
    Hspec.it "Should run successfully" $ do
      goodMockShell `shouldSatisfy` verifyGoodShell
    Hspec.it "Should die on legend error" $ do
      badLegendMockShell `shouldSatisfy` verifyBadLegendShell
    Hspec.it "Should continue with no legend" $ do
      noLegendMockShell `shouldSatisfy` verifyNoLegendShell

goodMockShell :: GoodMockShell ()
goodMockShell = ShellRun.runShell

verifyGoodShell :: GoodMockShell () -> Bool
verifyGoodShell (MkGoodMockShell (MkMockShellBase _ logs)) =
  logs == ["echo hi", "command 2", "command 1"]

badLegendMockShell :: BadLegendMockShell ()
badLegendMockShell = ShellRun.runShell

verifyBadLegendShell :: BadLegendMockShell () -> Bool
verifyBadLegendShell (MkBadLegendMockShell (MkMockShellBase _ logs)) =
  logs == ["\ESC[91m[Error] FileErr \"File not found\"\ESC[0m\n"]

noLegendMockShell :: NoLegendMockShell ()
noLegendMockShell = ShellRun.runShell

verifyNoLegendShell :: NoLegendMockShell () -> Bool
verifyNoLegendShell (MkNoLegendMockShell (MkMockShellBase _ logs)) =
  logs == ["cmd2", "cmd1"]