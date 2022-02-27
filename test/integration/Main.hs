{-# LANGUAGE OverloadedLists #-}

-- | Runs integration tests.
module Main (main) where

import Data.Functor.Identity (Identity (..))
import MockEnv (MockEnv (..))
import MockEnv qualified
import MockShell.BadLegendMockShell (BadLegendMockShell (..))
import MockShell.GoodMockShell (GoodMockShell (..))
import MockShell.MockShellBase (MockShellBase (..))
import MockShell.NoLegendMockShell (NoLegendMockShell (..))
import ShellRun qualified
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | Entry point for integration tests.
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Integration Tests"
      [ goodShell,
        goodShellDefaultLegend,
        badLegendShell,
        noLegendShell,
        noDefaultLegendShell
      ]

goodShell :: TestTree
goodShell = THU.testCase "Should run successfully" $ do
  let env =
        (MockEnv.defaultEnv ("echo hi" :|^ ["both"]))
          { legend = FPManual "path"
          }
      expected = ["echo hi", "command 1", "command 2"]
      result = getLogs runGoodMockShell ShellRun.runShell env

  expected @=? result

goodShellDefaultLegend :: TestTree
goodShellDefaultLegend = THU.testCase "Should run with default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "def-key"))
          { legend = FPDefault
          }
      expected = ["def-val"]
      result = getLogs runGoodMockShell ShellRun.runShell env
  expected @=? result

badLegendShell :: TestTree
badLegendShell = THU.testCase "Should die on legend error" $ do
  let env =
        (MockEnv.defaultEnv ("mock-cmd" :|^ []))
          { legend = FPManual "bad/path"
          }
      expected = ["Error parsing legend file: FileErr \"File not found\""]
      result = getLogs runBadLegendMockShell ShellRun.runShell env
  expected @=? result

noLegendShell :: TestTree
noLegendShell = THU.testCase "Should continue with no manual legend" $ do
  let env = MockEnv.defaultEnv ("cmd1" :|^ ["cmd2"])
      expected = ["cmd1", "cmd2"]
      result = getLogs runNoLegendMockShell ShellRun.runShell env

  expected @=? result

noDefaultLegendShell :: TestTree
noDefaultLegendShell = THU.testCase "Should continue with no default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "cmd"))
          { legend = FPDefault
          }
      logs = getLogs runNoLegendMockShell ShellRun.runShell env
      expected =
        [ "No legend file found at: \"config/legend.txt\"",
          "cmd"
        ]
  expected @=? logs

getLogs :: (t -> MockShellBase a) -> t -> MockEnv -> List Text
getLogs runMock mock env =
  let base = runMock mock
      rdr = runMockShellBase base
      wtr = runReaderT rdr env
      (Identity (_, logs)) = runWriterT wtr
   in logs
