-- | Functional tests for graph.
module Functional.Graph (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Functional.Prelude
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout (NotifyTimeoutSeconds))
import Test.Shrun.Verifier qualified as V

tests :: TestTree
tests =
  testGroup
    "Command graph"
    [ testCommandGraphSuccess,
      testCommandGraphSuccessOr,
      testCommandGraphSuccessAny,
      testCommandGraphRunsAtMostOnce,
      testCommandGraphComplex,
      testCommandGraphFailure,
      testCommandGraphBlockedFailure,
      testCommandGraphSeqAnd,
      testCommandGraphSeqOr,
      testCommandGraphSeqAny,
      testCommandGraphLegend,
      testCommandGraphLegendAndEdge,
      testCommandGraphLegendEdgeFailure,
      cancelSequential,
      timeoutSequential
    ]

testCommandGraphSuccess :: TestTree
testCommandGraphSuccess = testCase "Runs with --edges" $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    args =
      withNoConfig
        [ "--edges",
          "1 & 3, 2 & 3",
          "--common-log-debug",
          "on",
          "sleep 3.5",
          "sleep 2",
          "sleep 3",
          "sleep 4"
        ]

    expected =
      [ withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2" "Command 'sleep 3' is blocked due to dependency pending: '(1) sleep 3.5'.",
        withDebugNoCmdPrefix "Starting 'sleep 3.5'.",
        withDebugNoCmdPrefix "Starting 'sleep 2'.",
        withDebugNoCmdPrefix "Starting 'sleep 4'.",
        withDebugPrefix "sleep 3.5" "Starting 'sleep 3'."
      ]

    unexpected =
      [ withDebugNoCmdPrefix "Starting 'sleep 3'.",
        withDebugPrefix "sleep 2" "Starting 'sleep 3'."
      ]

testCommandGraphSuccessOr :: TestTree
testCommandGraphSuccessOr = testCase "Runs with or --edges" $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    args =
      withNoConfig
        [ "--edges",
          "1 & 3, 2 | 3, 2 & 5, 1 | 6",
          "--common-log-debug",
          "on",
          "sleep 3.5",
          "sleep 2 && bad",
          "sleep 3",
          "sleep 4",
          "sleep 5",
          "sleep 6"
        ]

    expected =
      [ withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2 && bad" "Command 'sleep 3' is blocked due to dependency pending: '(1) sleep 3.5'.",
        withErrorPrefix "sleep 2 && bad" <> "2 seconds",
        withErrorPrefix "sleep 2 && bad" <> "Not starting 'sleep 5' due to dependency failure: '(2) sleep 2 && bad'",
        withWarnCmdPrefix "sleep 3.5" <> "Not starting 'sleep 6' due to dependency success: '(1) sleep 3.5'",
        waitingPrefix,
        "  - sleep 5",
        "  - sleep 6",
        withDebugNoCmdPrefix "Starting 'sleep 3.5'.",
        withDebugNoCmdPrefix "Starting 'sleep 2 && bad'.",
        withDebugNoCmdPrefix "Starting 'sleep 4'.",
        withDebugPrefix "sleep 3.5" "Starting 'sleep 3'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 5",
        withSuccessPrefix "sleep 6",
        withDebugNoCmdPrefix "Starting 'sleep 3'.",
        withDebugPrefix "sleep 2" "Starting 'sleep 3'."
      ]

testCommandGraphSuccessAny :: TestTree
testCommandGraphSuccessAny = testCase "Runs with any --edges" $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    args =
      withNoConfig
        [ "--edges",
          "1 ; 3, 2 ; 3, 2 & 5",
          "--common-log-debug",
          "on",
          "sleep 3.5",
          "sleep 2 && bad",
          "sleep 3",
          "sleep 4",
          "sleep 5"
        ]

    expected =
      [ withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2 && bad" "Command 'sleep 3' is blocked due to dependency pending: '(1) sleep 3.5'.",
        withErrorPrefix "sleep 2 && bad" <> "2 seconds",
        withErrorPrefix "sleep 2 && bad" <> "Not starting 'sleep 5' due to dependency failure: '(2) sleep 2 && bad'",
        waitingPrefix,
        "  - sleep 5",
        withDebugNoCmdPrefix "Starting 'sleep 3.5'.",
        withDebugNoCmdPrefix "Starting 'sleep 2 && bad'.",
        withDebugNoCmdPrefix "Starting 'sleep 4'.",
        withDebugPrefix "sleep 3.5" "Starting 'sleep 3'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 5",
        withDebugNoCmdPrefix "Starting 'sleep 3'.",
        withDebugPrefix "sleep 2 && bad" "Starting 'sleep 3'.",
        withDebugPrefix "sleep 2 && bad" "Starting 'sleep 5'."
      ]

testCommandGraphRunsAtMostOnce :: TestTree
testCommandGraphRunsAtMostOnce = testCase desc $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedN resultsConsole expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 4") $ seconds > 4
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    desc = "Runs --edges commands at most once"

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
        [ "--edges",
          "1 & 4, 2 & 4, 3 & 4",
          "--common-log-debug",
          "on",
          "sleep 2",
          "sleep 2",
          "sleep 2",
          "sleep 3"
        ]

    expected =
      [ -- 3 because we have 3 actual commands
        (3, withSuccessPrefix "sleep 2"),
        -- There better only be one.
        (1, withSuccessPrefix "sleep 3")
      ]

testCommandGraphComplex :: TestTree
testCommandGraphComplex = testCase desc $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedN resultsConsole expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 6 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    desc = "Runs --edges commands with complex edges"

    args =
      withNoConfig
        [ "--edges",
          "{1,2} & {3..5,6} & 7 &.. 9 & {10, 11}",
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

    expected =
      [ (11, withSuccessPrefix "sleep 1")
      ]

testCommandGraphFailure :: TestTree
testCommandGraphFailure = testCase "Runs with --edges failure" $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take over 8 seconds.
  assertBool (show seconds ++ " > 3") $ seconds > 3
  assertBool (show seconds ++ " < 5") $ seconds < 5
  where
    args =
      withNoConfig
        [ "--edges",
          "1 & 3, 2 & 3",
          "--common-log-debug",
          "on",
          "sleep 3.5",
          "sleep 2 && sdf",
          "sleep 8",
          "sleep 4"
        ]

    expected =
      [ withSuccessPrefix "sleep 3.5",
        withSuccessPrefix "sleep 4",
        withErrorPrefix "sleep 2 && sdf" <> "Not starting 'sleep 8' due to dependency failure: '(2) sleep 2 && sdf'.",
        withErrorPrefix "sleep 3.5" <> "Not starting 'sleep 8' due to dependency failure: '(2) sleep 2 && sdf'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 2"
      ]

testCommandGraphBlockedFailure :: TestTree
testCommandGraphBlockedFailure = testCase desc $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  -- Without --edges this command should take over 8 seconds.
  assertBool (show seconds ++ " > 3") $ seconds > 3
  assertBool (show seconds ++ " < 5") $ seconds < 5
  where
    desc = "Tests that command is blocked with eventual failure"

    args =
      withNoConfig
        [ "--edges",
          "1 & 3, 2 & 3",
          "--common-log-debug",
          "on",
          "sleep 3.5 && sdf",
          "sleep 2",
          "sleep 8",
          "sleep 4"
        ]

    expected =
      [ withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 4",
        withDebugPrefix "sleep 2" "Command 'sleep 8' is blocked due to dependency pending: '(1) sleep 3.5 && sdf'",
        withErrorPrefix "sleep 3.5 && sdf" <> "Not starting 'sleep 8' due to dependency failure: '(1) sleep 3.5 && sdf'."
      ]

    unexpected =
      [ withSuccessPrefix "sleep 3.5"
      ]

testCommandGraphSeqAnd :: TestTree
testCommandGraphSeqAnd = testCase "Runs with --edges '&&'" $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpected resultsConsole expected

  let seconds = ts ^. #sec

  -- Without --edges this command should take around 4 seconds.
  assertBool (show seconds ++ " > 9") $ seconds > 9
  assertBool (show seconds ++ " < 11") $ seconds < 11
  where
    args =
      -- We are not actually testing --console-log-command other than the
      -- fact that it doesn't crash the program (i.e. the regionList logic
      -- doesn't explode).
      withNoConfig
        [ "--edges",
          "&&",
          "--common-log-debug",
          "on",
          "--console-log-command",
          "on",
          "sleep 1",
          "sleep 2",
          "sleep 3",
          "sleep 4"
        ]

    expected =
      [ withSuccessPrefix "sleep 1",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 4"
      ]

testCommandGraphSeqOr :: TestTree
testCommandGraphSeqOr = testCase "Runs with --edges '||'" $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpectedUnexpected resultsConsole expected unexpected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 2") $ seconds > 2
  assertBool (show seconds ++ " < 5") $ seconds < 5
  where
    args =
      withNoConfig
        [ "--edges",
          "||",
          "--common-log-debug",
          "on",
          "--console-log-command",
          "on",
          "sleep 1 && bad",
          "sleep 2",
          "sleep 3"
        ]

    expected =
      [ withErrorPrefix "sleep 1 && bad",
        withSuccessPrefix "sleep 2",
        withWarnCmdPrefix "sleep 2" <> "Not starting 'sleep 3' due to dependency success: '(2) sleep 2'"
      ]

    unexpected =
      [ withSuccessPrefix "sleep 3"
      ]

testCommandGraphSeqAny :: TestTree
testCommandGraphSeqAny = testCase "Runs with --edges ';;'" $ do
  (ts, resultsConsole) <- withTiming $ runExitFailure args

  V.verifyExpected resultsConsole expected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 7") $ seconds < 7
  where
    args =
      withNoConfig
        [ "--edges",
          ";;",
          "--common-log-debug",
          "on",
          "--console-log-command",
          "on",
          "sleep 1",
          "sleep 2 && bad",
          "sleep 3"
        ]

    expected =
      [ withSuccessPrefix "sleep 1",
        withErrorPrefix "sleep 2 && bad",
        withSuccessPrefix "sleep 3"
      ]

testCommandGraphLegend :: TestTree
testCommandGraphLegend = testCase "Runs with --legend edges" $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedOrder resultsConsole expected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 13") $ seconds > 13
  assertBool (show seconds ++ " < 15") $ seconds < 15
  where
    args =
      withNoConfig
        [ "-c",
          "test/functional/config.toml",
          "sleep 0",
          "edges1",
          "sleep 14",
          "edges2"
        ]

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

testCommandGraphLegendAndEdge :: TestTree
testCommandGraphLegendAndEdge = testCase desc $ do
  (ts, resultsConsole) <- withTiming $ run args

  V.verifyExpectedOrder resultsConsole expected

  let seconds = ts ^. #sec

  assertBool (show seconds ++ " > 18") $ seconds > 18
  assertBool (show seconds ++ " < 21") $ seconds < 21
  where
    desc = "Runs with --edges and legend edges"

    args =
      withNoConfig
        [ "-c",
          "test/functional/config.toml",
          "--edges",
          "1 & 3, 2 & 4",
          "sleep 0",
          "edges1",
          "sleep 14",
          "edges2"
        ]

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
  (_, ex) <- runExceptionE @StringException args

  "Key bad_edge: Index '3' in edge '1 & 3' is out-of-bounds." @=? displayException ex
  where
    desc = "Runs with legend edges failure"

    args =
      withNoConfig
        [ "-c",
          "test/functional/config.toml",
          "bad_edge"
        ]

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

cancelSequential :: TestTree
cancelSequential = testCase desc $ do
  (resultsConsole, notes) <- runCancelled 2 args

  V.verifyExpected resultsConsole expected

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

    args =
      withNoConfig
        [ "--notify-action-complete",
          "all",
          "--notify-system",
          notifySystemArg,
          "--console-log-command",
          "on",
          "--edges",
          "1 & 3, 3 & 4",
          "sleep 4",
          "sleep 8",
          "sleep 3",
          "sleep 5"
        ]

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
        withKilledPrefix (2, 2, 0, 0) expectedBody
      ]

    expectedBody :: (IsString s) => s
    expectedBody = "Received cancel after running for"

timeoutSequential :: TestTree
timeoutSequential = testCase desc $ do
  (resultsConsole, notes) <- runAllExitFailure args
  V.verifyExpected resultsConsole expected

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

    args =
      withNoConfig
        [ "--notify-action-complete",
          "all",
          "--notify-system",
          notifySystemArg,
          "--console-log-command",
          "on",
          "--edges",
          "1 & 3, 3 & 4",
          "sleep 4",
          "sleep 8",
          "sleep 3",
          "sleep 5",
          "-t",
          "2"
        ]

    -- see NOTE: [Cancelled tasks Warn vs. Fatal]
    expected =
      [ waitingPrefix,
        "  - sleep 3",
        "  - sleep 5",
        runningPrefix,
        "  - sleep 4",
        "  - sleep 8",
        timedOut,
        withFinishedPrefix (2, 2, 0, 0) "3 seconds"
      ]
