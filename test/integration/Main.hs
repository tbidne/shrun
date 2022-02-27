{-# LANGUAGE OverloadedLists #-}

-- | Runs integration tests.
module Main (main) where

import Integration.MockEnv (MockEnv (..))
import Integration.MockEnv qualified as MockEnv
import Integration.MockShell.BadLegendMockShell (runBadLegendMockShell)
import Integration.MockShell.GoodMockShell (runGoodMockShell)
import Integration.MockShell.NoLegendMockShell (runNoLegendMockShell)
import Integration.Prelude
import ShellRun qualified
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import Test.Tasty qualified as Tasty
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
      (_, result) = runGoodMockShell ShellRun.runShell env

  expected @=? result

goodShellDefaultLegend :: TestTree
goodShellDefaultLegend = THU.testCase "Should run with default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "def-key"))
          { legend = FPDefault
          }
      expected = ["def-val"]
      (_, result) = runGoodMockShell ShellRun.runShell env
  expected @=? result

badLegendShell :: TestTree
badLegendShell = THU.testCase "Should die on legend error" $ do
  let env =
        (MockEnv.defaultEnv ("mock-cmd" :|^ []))
          { legend = FPManual "bad/path"
          }
      expected = ["Error parsing legend file: FileErr \"File not found\""]
      (_, result) = runBadLegendMockShell ShellRun.runShell env
  expected @=? result

noLegendShell :: TestTree
noLegendShell = THU.testCase "Should continue with no manual legend" $ do
  let env = MockEnv.defaultEnv ("cmd1" :|^ ["cmd2"])
      expected = ["cmd1", "cmd2"]
      (_, result) = runNoLegendMockShell ShellRun.runShell env

  expected @=? result

noDefaultLegendShell :: TestTree
noDefaultLegendShell = THU.testCase "Should continue with no default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "cmd"))
          { legend = FPDefault
          }
      (_, result) = runNoLegendMockShell ShellRun.runShell env
      expected =
        [ "No legend file found at: \"config/legend.txt\"",
          "cmd"
        ]
  expected @=? result
