{-# LANGUAGE QuasiQuotes #-}

-- | Functional tests for graph.
module Functional.Graph (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout (NotifyTimeoutSeconds))
import Test.Shrun.Verifier qualified as V

tests :: IO TestArgs -> TestTree
tests testArgs =
  testGroup
    "Command graph"
    [ testCommandGraphSuccess testArgs,
      testCommandGraphRunsAtMostOnce testArgs,
      testCommandGraphComplex testArgs,
      testCommandGraphFailure testArgs,
      testCommandGraphBlockedFailure testArgs,
      testCommandGraphSequential testArgs,
      testCommandGraphLegend testArgs,
      testCommandGraphLegendAndEdge testArgs,
      testCommandGraphLegendEdgeFailure,
      cancelSequential testArgs,
      timeoutSequential testArgs
    ]

testCommandGraphSuccess :: IO TestArgs -> TestTree
testCommandGraphSuccess testArgs = testCase "Runs with --edges" $ do
  outFile <- (</> [osp|testCommandGraphSuccess.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
            "on",
            "sleep 3.5",
            "sleep 2",
            "sleep 3",
            "sleep 4"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpected resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    expected =
      [ withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2" "Command 'sleep 3' is blocked due to dependency pending: '(1) sleep 3.5'."
      ]

testCommandGraphRunsAtMostOnce :: IO TestArgs -> TestTree
testCommandGraphRunsAtMostOnce testArgs = testCase desc $ do
  outFile <- (</> [osp|testCommandGraphRunsAtMostOnce.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      -- Having three commands of the same duration gives us a pretty good
      -- chance that all three threads will finish simultaneously and attempt
      -- to kick off the 'sleep 3' command.
      --
      -- This is actually pretty reliable. Without the specific prevention
      -- logic, this tends to spawn 3 'sleep 3' commands as expected, hence
      -- fails.
      --
      -- See NOTE: [Command Race] for the logic.
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "1 -> 4, 2 -> 4, 3 -> 4",
            "--common-log-debug",
            "on",
            "sleep 2",
            "sleep 2",
            "sleep 2",
            "sleep 3"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedN resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpectedN resultsFile expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 4") $ seconds > 4
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    desc = "Runs --edges commands at most once"
    expected =
      [ -- 3 because we have 3 actual commands
        (3, withSuccessPrefix "sleep 2"),
        -- There better only be one.
        (1, withSuccessPrefix "sleep 3")
      ]

testCommandGraphComplex :: IO TestArgs -> TestTree
testCommandGraphComplex testArgs = testCase desc $ do
  outFile <- (</> [osp|testCommandGraphComplex.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "{1,2} -> {3..5,6} -> 7 .. 9 -> {10, 11}",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1",
            "sleep 1"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedN resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpectedN resultsFile expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 6 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    desc = "Runs --edges commands with complex edges"
    expected =
      [ (11, withSuccessPrefix "sleep 1")
      ]

testCommandGraphFailure :: IO TestArgs -> TestTree
testCommandGraphFailure testArgs = testCase "Runs with --edges failure" $ do
  outFile <- (</> [osp|testCommandGraphFailure.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
            "on",
            "sleep 3.5",
            "sleep 2 && sdf",
            "sleep 8",
            "sleep 4"
          ]

  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  resultsFile <- readLogFile outFile
  V.verifyExpectedUnexpected resultsFile expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take over 8 seconds.
  assertBool (show seconds ++ " > 3") $ seconds > 3
  assertBool (show seconds ++ " < 5") $ seconds < 5
  where
    expected =
      [ withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withErrorPrefix "sleep 2 && sdf" <> "Not running 'sleep 8' due to dependency failure: '(2) sleep 2 && sdf'.",
        withErrorPrefix "sleep 3.5" <> "Not running 'sleep 8' due to dependency failure: '(2) sleep 2 && sdf'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 2"
      ]

testCommandGraphBlockedFailure :: IO TestArgs -> TestTree
testCommandGraphBlockedFailure testArgs = testCase desc $ do
  outFile <- (</> [osp|testCommandGraphFailure.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
            "on",
            "sleep 3.5 && sdf",
            "sleep 2",
            "sleep 8",
            "sleep 4"
          ]

  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  resultsFile <- readLogFile outFile
  V.verifyExpectedUnexpected resultsFile expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take over 8 seconds.
  assertBool (show seconds ++ " > 3") $ seconds > 3
  assertBool (show seconds ++ " < 5") $ seconds < 5
  where
    desc = "Tests that command is blocked with eventual failure"
    expected =
      [ withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2" "Command 'sleep 8' is blocked due to dependency pending: '(1) sleep 3.5 && sdf'",
        withErrorPrefix "sleep 3.5 && sdf" <> "Not running 'sleep 8' due to dependency failure: '(1) sleep 3.5 && sdf'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 3.5"
      ]

testCommandGraphSequential :: IO TestArgs -> TestTree
testCommandGraphSequential testArgs = testCase "Runs with --edges sequential" $ do
  outFile <- (</> [osp|testCommandGraphSequential.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        -- We are not actually testing --console-log-command other than the
        -- fact that it doesn't crash the program (i.e. the regionList logic
        -- doesn't explode).
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--edges",
            "sequential",
            "--common-log-debug",
            "on",
            "--console-log-command",
            "on",
            "sleep 1",
            "sleep 2",
            "sleep 3",
            "sleep 4"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpected resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 9") $ seconds > 9
  assertBool (show seconds ++ " < 11") $ seconds < 11
  where
    expected =
      [ withSuccessPrefix "sleep 1",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 4"
      ]

testCommandGraphLegend :: IO TestArgs -> TestTree
testCommandGraphLegend testArgs = testCase "Runs with --legend edges" $ do
  outFile <- (</> [osp|testCommandGraphLegend.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "-c",
            "test/functional/config.toml",
            "--file-log",
            outFileStr,
            "sleep 0",
            "edges1",
            "sleep 14",
            "edges2"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedOrder resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpectedOrder resultsFile expected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 13") $ seconds > 13
  assertBool (show seconds ++ " < 15") $ seconds < 15
  where
    expected =
      [ withSuccessPrefix "sleep 0",
        withSuccessPrefix "e11",
        withSuccessPrefix "e21",
        withSuccessPrefix "e121",
        withSuccessPrefix "e22",
        withSuccessPrefix "e122",
        withSuccessPrefix "e23",
        withSuccessPrefix "e131",
        withSuccessPrefix "e132",
        withSuccessPrefix "e133",
        withSuccessPrefix "e14",
        withSuccessPrefix "sleep 14"
      ]

testCommandGraphLegendAndEdge :: IO TestArgs -> TestTree
testCommandGraphLegendAndEdge testArgs = testCase desc $ do
  outFile <- (</> [osp|testCommandGraphLegendAndEdge.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "-c",
            "test/functional/config.toml",
            "--file-log",
            outFileStr,
            "--edges",
            "1 -> 3, 2 -> 4",
            "sleep 0",
            "edges1",
            "sleep 14",
            "edges2"
          ]

  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedOrder resultsConsole expected

  resultsFile <- readLogFile outFile
  V.verifyExpectedOrder resultsFile expected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 18") $ seconds > 18
  assertBool (show seconds ++ " < 21") $ seconds < 21
  where
    desc = "Runs with --edges and legend edges"

    -- Same as the previous example, except the e2s are after the e1s.
    -- Hence the e2s are moved to the end. Hopefully this is still
    -- deterministic (have not verified).
    expected =
      [ withSuccessPrefix "sleep 0",
        withSuccessPrefix "e11",
        withSuccessPrefix "e121",
        withSuccessPrefix "e122",
        withSuccessPrefix "e131",
        withSuccessPrefix "e132",
        withSuccessPrefix "e133",
        withSuccessPrefix "e14",
        withSuccessPrefix "sleep 14",
        withSuccessPrefix "e21",
        withSuccessPrefix "e22",
        withSuccessPrefix "e23"
      ]

testCommandGraphLegendEdgeFailure :: TestTree
testCommandGraphLegendEdgeFailure = testCase desc $ do
  let args =
        withNoConfig
          [ "-c",
            "test/functional/config.toml",
            "bad_edge"
          ]

  (_, ex) <- runExceptionE @StringException args

  "Index '3' in edge '1 -> 3' is out-of-bounds." @=? displayException ex
  where
    desc = "Runs with legend edges failure"

-- NOTE:
--
-- cancelSequential and timeoutSequential _are_ parametric wrt to
-- read strategy, but they involve multiple commands w/ file logs, so we
-- can only use the 'block' read strategy, hence cannot be
-- ReadStrategyTestParams.
--
-- Essentially, we can choose one of the following:
--
--   1. Test file logs.
--   2. Test read strategy.
--
-- As these tests have nothing to do with read strategy, we choose 1.

cancelSequential :: IO TestArgs -> TestTree
cancelSequential testArgs = testCase desc $ do
  outFile <- (</> [osp|cancelSequential.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--notify-action",
            "all",
            "--notify-system",
            notifySystemArg,
            "--console-log-command",
            "on",
            "--edges",
            "1 -> 3, 3 -> 4",
            "sleep 4",
            "sleep 8",
            "sleep 3",
            "sleep 5"
          ]

  (resultsConsole, notes) <- runCancelled 2 args

  V.verifyExpected resultsConsole expected

  fileResults <- readLogFile outFile
  V.verifyExpected fileResults expected

  case notes of
    [n] -> do
      "" @=? n ^. #body

      let summary = n ^. (#summary % #unNotifyMessage)
          err = "Unexpected summary: " ++ unpack summary

      assertBool err $ expectedBody `T.isPrefixOf` summary

      NotifyTimeoutSeconds 10 @=? n ^. #timeout
      Critical @=? n ^. #urgency
    other ->
      assertFailure
        $ "Expected exactly one note, received: "
        ++ show other
  where
    desc = "Shrun is cancelled with sequential tasks"

    -- NOTE: [Cancelled tasks Warn vs. Fatal]
    --
    -- These are virtually identical to timeoutSequential, the difference
    -- being warn vs. fatal.
    expected =
      [ waitingPrefix,
        "  - sleep 3",
        "  - sleep 5",
        runningPrefix,
        "  - sleep 4",
        "  - sleep 8",
        withKilledPrefix expectedBody
      ]

    expectedBody :: (IsString s) => s
    expectedBody = "Received cancel after running for"

timeoutSequential :: IO TestArgs -> TestTree
timeoutSequential testArgs = testCase desc $ do
  outFile <- (</> [osp|timeoutSequential.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--notify-action",
            "all",
            "--notify-system",
            notifySystemArg,
            "--console-log-command",
            "on",
            "--edges",
            "1 -> 3, 3 -> 4",
            "sleep 4",
            "sleep 8",
            "sleep 3",
            "sleep 5",
            "-t",
            "2"
          ]
  (resultsConsole, notes) <- runAllExitFailure args
  V.verifyExpected resultsConsole expected

  fileResults <- readLogFile outFile
  V.verifyExpected fileResults expected

  case notes of
    [n] -> do
      "3 seconds" @=? n ^. #body

      let summary = n ^. (#summary % #unNotifyMessage)
          err = "Unexpected summary: " ++ unpack summary

      assertBool err $ "Shrun Finished" `T.isPrefixOf` summary

      NotifyTimeoutSeconds 10 @=? n ^. #timeout
      Critical @=? n ^. #urgency
    other ->
      assertFailure
        $ "Expected exactly one note, received: "
        ++ show other
  where
    desc = "Shrun times out with sequential tasks"

    -- see NOTE: [Cancelled tasks Warn vs. Fatal]
    expected =
      [ waitingPrefix,
        "  - sleep 3",
        "  - sleep 5",
        runningPrefix,
        "  - sleep 4",
        "  - sleep 8",
        timedOut,
        withFinishedPrefix "3 seconds"
      ]
