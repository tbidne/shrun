{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad.Reader qualified as MTL
import Control.Monad.Writer qualified as MTL
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import MockShell.BadLegendMockShell (BadLegendMockShell (..))
import MockShell.GoodMockShell (GoodMockShell (..))
import MockShell.MockShellBase (MockShellBase (..))
import MockShell.NoLegendMockShell (NoLegendMockShell (..))
import ShellRun qualified
import ShellRun.Types.Env (Env (..))
import ShellRun.Types.Env qualified as Env
import Test.Hspec (Spec, shouldBe)
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
      let (logs, b) = verifyGoodShell goodMockShell
      if not b
        then Hspec.expectationFailure $ show logs
        else True `shouldBe` True
    Hspec.it "Should die on legend error" $ do
      let (logs, b) = verifyBadLegendShell badLegendMockShell
      if not b
        then Hspec.expectationFailure $ show logs
        else True `shouldBe` True
    Hspec.it "Should continue with no legend" $ do
      let (logs, b) = verifyNoLegendShell noLegendMockShell
      if not b
        then Hspec.expectationFailure $ show logs
        else True `shouldBe` True

goodMockShell :: GoodMockShell ()
goodMockShell = ShellRun.runShell

verifyGoodShell :: GoodMockShell () -> ([Text], Bool)
verifyGoodShell gms =
  let env =
        Env.defaultEnv
          { commands = ["echo hi", "both"],
            legend = Just "path"
          }
      logs = getLogs runGoodMockShell gms env
   in (logs, logs == ["echo hi", "command 1", "command 2"])

badLegendMockShell :: BadLegendMockShell ()
badLegendMockShell = ShellRun.runShell

verifyBadLegendShell :: BadLegendMockShell () -> ([Text], Bool)
verifyBadLegendShell blms =
  let env =
        Env.defaultEnv
          { legend = Just "bad/path"
          }
      logs = getLogs runBadLegendMockShell blms env
   in (logs, logs == ["\ESC[91m[Error] Error parsing legend file: FileErr \"File not found\"\ESC[0m\n"])

noLegendMockShell :: NoLegendMockShell ()
noLegendMockShell = ShellRun.runShell

verifyNoLegendShell :: NoLegendMockShell () -> ([Text], Bool)
verifyNoLegendShell nlms =
  let env =
        Env.defaultEnv
          { commands = ["cmd1", "cmd2"]
          }
      logs = getLogs runNoLegendMockShell nlms env
   in (logs, logs == ["cmd1", "cmd2"])

getLogs :: (t -> MockShellBase a) -> t -> Env -> [Text]
getLogs runMock mock env =
  let base = runMock mock
      rdr = runMockShellBase base
      wtr = MTL.runReaderT rdr env
      (Identity (_, logs)) = MTL.runWriterT wtr
   in logs