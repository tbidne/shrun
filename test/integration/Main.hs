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
  let (logs, b) = verifyGoodShell goodMockShell
  THU.assertBool (show logs) b

goodShellDefaultLegend :: TestTree
goodShellDefaultLegend = THU.testCase "Should run with default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "def-key"))
          { legend = FPDefault
          }
      logs = getLogs runGoodMockShell goodMockShell env
  ["def-val"] @=? logs

badLegendShell :: TestTree
badLegendShell = THU.testCase "Should die on legend error" $ do
  let (logs, b) = verifyBadLegendShell badLegendMockShell
  THU.assertBool (show logs) b

noLegendShell :: TestTree
noLegendShell = THU.testCase "Should continue with no manual legend" $ do
  let (logs, b) = verifyNoLegendShell noLegendMockShell
  THU.assertBool (show logs) b

noDefaultLegendShell :: TestTree
noDefaultLegendShell = THU.testCase "Should continue with no default legend" $ do
  let env =
        (MockEnv.defaultEnv (NESeq.singleton "cmd"))
          { legend = FPDefault
          }
      logs = getLogs runNoLegendMockShell noLegendMockShell env
      expected =
        [ "No legend file found at: \"config/legend.txt\"",
          "cmd"
        ]
  expected @=? logs

goodMockShell :: GoodMockShell ()
goodMockShell = ShellRun.runShell

verifyGoodShell :: GoodMockShell () -> (List Text, Bool)
verifyGoodShell gms =
  let env =
        (MockEnv.defaultEnv ("echo hi" :|^ ["both"]))
          { legend = FPManual "path"
          }
      logs = getLogs runGoodMockShell gms env
   in (logs, logs == ["echo hi", "command 1", "command 2"])

badLegendMockShell :: BadLegendMockShell ()
badLegendMockShell = ShellRun.runShell

verifyBadLegendShell :: BadLegendMockShell () -> (List Text, Bool)
verifyBadLegendShell blms =
  let env =
        (MockEnv.defaultEnv ("mock-cmd" :|^ []))
          { legend = FPManual "bad/path"
          }
      logs = getLogs runBadLegendMockShell blms env
   in (logs, logs == ["Error parsing legend file: FileErr \"File not found\""])

noLegendMockShell :: NoLegendMockShell ()
noLegendMockShell = ShellRun.runShell

verifyNoLegendShell :: NoLegendMockShell () -> (List Text, Bool)
verifyNoLegendShell nlms =
  let env = MockEnv.defaultEnv ("cmd1" :|^ ["cmd2"])
      logs = getLogs runNoLegendMockShell nlms env
   in (logs, logs == ["cmd1", "cmd2"])

getLogs :: (t -> MockShellBase a) -> t -> MockEnv -> List Text
getLogs runMock mock env =
  let base = runMock mock
      rdr = runMockShellBase base
      wtr = runReaderT rdr env
      (Identity (_, logs)) = runWriterT wtr
   in logs
