{-# LANGUAGE OverloadedLists #-}

-- | Runs integration tests. These do real IO, but we do not concern ourselves
-- with the gritty output details. These primarily test various legend paths.
--
-- @since 0.1
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
import ShellRun.Effects.MonadFSReader (MonadFSReader)
import ShellRun.Env (HasLogging)
import ShellRun.Env.Types (HasCommands, HasCompletedCmds, HasLegend, HasTimeout)
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import System.Console.Regions (ConsoleRegion)
import Test.ShellRun.Verifier (ResultText (..))
import Test.ShellRun.Verifier qualified as Verifier
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
  env <-
    set' #legend (FPManual "path")
      <$> MockEnv.defaultEnv ("echo hi" :|^ ["both"])
  let expected =
        [ "[Info] [cmd1] Success.",
          "[Info] [cmd2] Success.",
          "[Info] [echo hi] Success.",
          "[Info] Finished!"
        ]

  result <- readTVarIO =<< runShellGetLogs runGoodMockShell env

  Verifier.verifyExpected (MkResultText <$> result) expected

goodShellDefaultLegend :: TestTree
goodShellDefaultLegend = THU.testCase "Should run with default legend" $ do
  env <-
    set' #legend FPDefault
      <$> MockEnv.defaultEnv (NESeq.singleton "def-key")
  let expected = ["def-val"]

  result <- readTVarIO =<< runShellGetLogs runGoodMockShell env

  Verifier.verifyExpected (MkResultText <$> result) expected

badLegendShell :: TestTree
badLegendShell = THU.testCase "Should die on legend error" $ do
  env <-
    set' #legend (FPManual "bad/path")
      <$> MockEnv.defaultEnv ("mock-cmd" :|^ [])
  let expected =
        [ "FileErr \"Error reading legend file `bad/path`",
          "File not found"
        ]

  result <- readTVarIO =<< runShellGetLogs runBadLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> result) expected

noLegendShell :: TestTree
noLegendShell = THU.testCase "Should continue with no manual legend" $ do
  env <- MockEnv.defaultEnv ("cmd1" :|^ ["cmd2"])
  let expected = ["[cmd1]", "[cmd2]"]

  result <- readTVarIO =<< runShellGetLogs runNoLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> result) expected

noDefaultLegendShell :: TestTree
noDefaultLegendShell = THU.testCase "Should continue with no default legend" $ do
  env <-
    set' #legend FPDefault
      <$> MockEnv.defaultEnv (NESeq.singleton "cmd")

  let expected =
        [ "No legend file found at: \"config/shell-run.legend\"",
          "[cmd]"
        ]

  result <- readTVarIO =<< runShellGetLogs runNoLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> result) expected

runShellGetLogs ::
  ( HasCommands env,
    HasCompletedCmds env,
    HasLegend env,
    HasLogging env,
    HasTimeout env,
    MonadFSReader m,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  (m () -> MockEnv -> IO a) ->
  MockEnv ->
  IO (TVar [Text])
runShellGetLogs mockApp env = do
  mockApp ShellRun.runShell env
  pure $ env ^. #logs
