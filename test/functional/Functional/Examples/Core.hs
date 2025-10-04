{-# LANGUAGE QuasiQuotes #-}

-- | Functional tests for readme examples.
module Functional.Examples.Core (tests) where

import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: IO TestArgs -> TestTree
tests testArgs =
  testGroup
    "CoreConfig"
    [ testCommandGraphSuccess testArgs,
      testCommandGraphRunsAtMostOnce testArgs,
      testCommandGraphComplex testArgs,
      testCommandGraphFailure testArgs,
      testCommandGraphBlockedFailure testArgs,
      testCommandGraphSequential testArgs,
      initOn,
      initOff,
      timeout
    ]

testCommandGraphSuccess :: IO TestArgs -> TestTree
testCommandGraphSuccess testArgs = testCase "Runs with --command-graph" $ do
  outFile <- (</> [osp|testCommandGraphSuccess.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--command-graph",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
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

  -- Without --command-graph this command should take around 4 seconds.
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
            "--command-graph",
            "1 -> 4, 2 -> 4, 3 -> 4",
            "--common-log-debug",
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

  -- Without --command-graph this command should take around 4 seconds.
  assertBool (show seconds ++ " > 4") $ seconds > 4
  assertBool (show seconds ++ " < 8") $ seconds < 8
  where
    desc = "Runs --command-graph commands at most once"
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
            "--command-graph",
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

  -- Without --command-graph this command should take around 6 seconds.
  assertBool (show seconds ++ " > 5") $ seconds > 5
  assertBool (show seconds ++ " < 7") $ seconds < 7
  where
    desc = "Runs --command-graph commands with complex edges"
    expected =
      [ (11, withSuccessPrefix "sleep 1")
      ]

testCommandGraphFailure :: IO TestArgs -> TestTree
testCommandGraphFailure testArgs = testCase "Runs with --command-graph failure" $ do
  outFile <- (</> [osp|testCommandGraphFailure.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--command-graph",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
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

  -- Without --command-graph this command should take over 8 seconds.
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
            "--command-graph",
            "1 -> 3, 2 -> 3",
            "--common-log-debug",
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

  -- Without --command-graph this command should take over 8 seconds.
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
testCommandGraphSequential testArgs = testCase "Runs with --command-graph sequential" $ do
  outFile <- (</> [osp|testCommandGraphSequential.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        -- We are not actually testing --console-log-command other than the
        -- fact that it doesn't crash the program (i.e. the regionList logic
        -- doesn't explode).
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--command-graph",
            "sequential",
            "--common-log-debug",
            "--console-log-command",
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

  -- Without --command-graph this command should take around 4 seconds.
  assertBool (show seconds ++ " > 9") $ seconds > 9
  assertBool (show seconds ++ " < 11") $ seconds < 11
  where
    expected =
      [ withSuccessPrefix "sleep 1",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "sleep 3",
        withSuccessPrefix "sleep 4"
      ]

initOn :: TestTree
initOn =
  testCase "Runs init successful example" $ do
    results <- run args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--init",
          ". examples/bashrc",
          "bash_function"
        ]
    expected =
      [ withSuccessPrefix "bash_function",
        finishedPrefix
      ]

initOff :: TestTree
initOff =
  testCase "Runs init failure example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "bash_function"
        ]
    expected =
      [ withErrorPrefix "bash_function",
        finishedPrefix
      ]

timeout :: TestTree
timeout =
  testCase "Runs timeout example" $ do
    results <- runExitFailure args
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-t",
          "4",
          "sleep 2",
          "sleep 6",
          "sleep 8"
        ]
    expected =
      [ withSuccessPrefix "sleep 2",
        runningPrefix,
        "  - sleep 6",
        "  - sleep 8",
        timedOut,
        finishedPrefix
      ]
