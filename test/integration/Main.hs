{-# LANGUAGE OverloadedLists #-}

-- | Runs integration tests. These do real IO, but we do not concern ourselves
-- with the gritty output details. These primarily test various legend paths.
--
-- @since 0.1
module Main (main) where

import Integration.IntEnv (IntEnv (..))
import Integration.IntEnv qualified as IntEnv
import Integration.MockShell.BadLegendMockShell (runBadLegendMockShell)
import Integration.MockShell.GoodMockShell (runGoodMockShell)
import Integration.MockShell.NoLegendMockShell (runNoLegendMockShell)
import Integration.Prelude
import ShellRun qualified
import ShellRun.Command (Command (..))
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Effects.MonadFSReader (MonadFSReader)
import ShellRun.Effects.MonadProcRunner (MonadProcRunner)
import ShellRun.Effects.MonadTime (MonadTime)
import ShellRun.Env (HasLogging)
import ShellRun.Env.Types (HasCommands, HasCompletedCmds, HasLegend, HasTimeout)
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
      <$> IntEnv.defaultEnv ("echo hi" :|^ ["both"])
  let expectedLogs =
        [ "[Info] [cmd1] Success.",
          "[Info] [cmd2] Success.",
          "[Info] [echo hi] Success.",
          "[Info] Finished!"
        ]
      expectedCmds =
        [ "cmd 1",
          "cmd 2",
          "echo hi"
        ]

  (resultLogs, resultCmds) <- runShellGetLogs runGoodMockShell env

  Verifier.verifyExpected (MkResultText <$> resultLogs) expectedLogs
  Verifier.verifyExpected (view cmdResultIso <$> resultCmds) expectedCmds

goodShellDefaultLegend :: TestTree
goodShellDefaultLegend = THU.testCase "Should run with default legend" $ do
  env <-
    set' #legend FPDefault
      <$> IntEnv.defaultEnv (NESeq.singleton "def-key")
  let expectedLogs = ["def-key"]
      expectedCmds = ["def-val"]

  (resultLogs, resultCmds) <- runShellGetLogs runGoodMockShell env

  Verifier.verifyExpected (MkResultText <$> resultLogs) expectedLogs
  Verifier.verifyExpected (view cmdResultIso <$> resultCmds) expectedCmds

badLegendShell :: TestTree
badLegendShell = THU.testCase "Should die on legend error" $ do
  env <-
    set' #legend (FPManual "bad/path")
      <$> IntEnv.defaultEnv ("mock-cmd" :|^ [])
  let expectedLogs =
        [ "FileErr \"Error reading legend file `bad/path`",
          "File not found"
        ]
      unexpectedCmds = ["mock-cmd"]

  (resultLogs, resultCmds) <- runShellGetLogs runBadLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> resultLogs) expectedLogs
  Verifier.verifyUnexpected (view cmdResultIso <$> resultCmds) unexpectedCmds

noLegendShell :: TestTree
noLegendShell = THU.testCase "Should continue with no manual legend" $ do
  env <- IntEnv.defaultEnv ("cmd1" :|^ ["cmd2"])
  let expectedLogs = ["[cmd1]", "[cmd2]"]
      expectedCmds = ["cmd1", "cmd2"]

  (resultLogs, resultCmds) <- runShellGetLogs runNoLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> resultLogs) expectedLogs
  Verifier.verifyExpected (view cmdResultIso <$> resultCmds) expectedCmds

noDefaultLegendShell :: TestTree
noDefaultLegendShell = THU.testCase "Should continue with no default legend" $ do
  env <-
    set' #legend FPDefault
      <$> IntEnv.defaultEnv (NESeq.singleton "cmd")

  let expectedLogs =
        [ "No legend file found at: \"config/shell-run.legend\"",
          "[cmd]"
        ]
      expectedCmds =
        [ "cmd"
        ]

  (resultLogs, resultCmds) <- runShellGetLogs runNoLegendMockShell env

  Verifier.verifyExpected (MkResultText <$> resultLogs) expectedLogs
  Verifier.verifyExpected (view cmdResultIso <$> resultCmds) expectedCmds

runShellGetLogs ::
  ( HasCommands env,
    HasCompletedCmds env,
    HasLegend env,
    HasLogging env,
    HasTimeout env,
    MonadFSReader m,
    MonadMask m,
    MonadProcRunner m,
    MonadReader env m,
    MonadTime m,
    MonadUnliftIO m
  ) =>
  (m () -> IntEnv -> IO a) ->
  IntEnv ->
  IO ([Text], [Command])
runShellGetLogs mockApp env = do
  mockApp ShellRun.runShell env
  logs' <- readTVarIO $ env ^. #logs
  cmdsRun' <- readTVarIO $ env ^. #cmdsRun
  pure (logs', cmdsRun')

cmdResultIso :: Iso' Command ResultText
cmdResultIso = iso (MkResultText . view #command) (MkCommand Nothing . view #getResultText)
