{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Misc tests
module Functional.Miscellaneous (specs) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout (NotifyTimeoutSeconds))
import Test.Shrun.Verifier (ExpectedText)
import Test.Shrun.Verifier qualified as V

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    (readStrategyDefaultTests testArgs : readStrategyTests)
  where
    readStrategyTests =
      multiTestReadStrategy testsParams

    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ splitNewlineLogs,
        formatErrorLogs testArgs,
        stripControlAlwaysCmdNames,
        reportsStderr,
        isCancelled testArgs,
        slowOutputBroken,
        formatsFileLogs testArgs
      ]

splitNewlineLogs :: ReadStrategyTestParams
splitNewlineLogs =
  ReadStrategyTestParametricSimple
    "Logs with newlines are split"
    run
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "on",
          "sleep 1 && echo 'line one\nline two' && sleep 2"
        ]

    -- Newline is stripped from printed result
    printedCmd :: (IsString a) => a
    printedCmd = "sleep 1 && echo 'line one line two' && sleep 2"

    expected =
      [ withSuccessPrefix printedCmd,
        withCommandPrefix printedCmd "line one",
        withCommandPrefix printedCmd "line two"
      ]
    unexpected =
      [ withCommandPrefix printedCmd "line one line two"
      ]

formatErrorLogs :: IO TestArgs -> ReadStrategyTestParams
formatErrorLogs testArgs =
  ReadStrategyTestParametricSetup
    "Error Log with newlines is preserved"
    runAllExitFailure
    ( \_ -> do
        outFile <- (</> [osp|file-log-errors.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "on",
                  "sleep 1 && echo 'abc\n  def' && sleep 1 && exit 1"
                ]
        pure (args, outFile)
    )
    ( \((resultsConsole, _), outFile) -> do
        V.verifyExpectedUnexpected resultsConsole expectedConsole unexpected

        fileResults <- readLogFile outFile
        V.verifyExpectedUnexpected fileResults expectedFile unexpected
    )
  where
    -- Verifying how final 'abc\n  def' log is translated in the final error
    -- msg. Breakdown:
    --
    -- - In command names, newlines are converted to spaces, so 'abc\n  def'
    --   'abc   def'.
    --
    -- - In command logs, newlines are split across separate logs. Hence
    --   'abc' and '  def'.
    --
    -- - In the final error message, newlines are preserved.
    expectedCommon =
      [ withErrorPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" <> "2 seconds",
        "  abc",
        "    def"
      ]
    expectedConsole =
      expectedCommon
        ++ [ withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "abc",
             withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "def"
           ]
    expectedFile =
      expectedCommon
        ++ [ withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "abc",
             withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "  def"
           ]
    unexpected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && sleep 1 && exit 1" <> "2 seconds: abcdef"
      ]

isCancelled :: IO TestArgs -> ReadStrategyTestParams
isCancelled testArgs =
  ReadStrategyTestParametricSetup
    "Shrun is cancelled"
    (runCancelled 2)
    ( \_ -> do
        outFile <- (</> [osp|cancelled.log|]) . view #tmpDir <$> testArgs
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
                  "sleep 5"
                ]
        pure (args, outFile)
    )
    ( \((resultsConsole, notes), outFile) -> do
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
    )
  where
    expected =
      [ runningPrefix,
        "  - sleep 5",
        withKilledPrefix expectedBody
      ]

    expectedBody :: (IsString s) => s
    expectedBody = "Received cancel after running for"

-- NOTE: This used to be in Examples (subsequently Examples.ConsoleLogging),
-- as it fit in alongside the other tests. However, we prefer those tests to
-- match Configuration.md as closely as possible, to make maintaining the
-- markdown file as easy as possible. Thus we move it here.
stripControlAlwaysCmdNames :: ReadStrategyTestParams
stripControlAlwaysCmdNames =
  ReadStrategyTestParametricSimple
    "Always strips command names"
    run
    args
    (`V.verifyExpected` expected)
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "on",
          "--console-log-strip-control",
          "off",
          "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
        ]
    -- i.e. ansi codes are not being stripped (because =none), yet they are
    -- gone from the command names
    expected =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 2" "foo \ESC[35m hello \ESC[3D bye",
        withSuccessPrefix "printf ' foo  hello  bye '; sleep 2"
      ]

-- Tests that we default to stderr when it exists.
-- See NOTE: [Stderr reporting].
reportsStderr :: ReadStrategyTestParams
reportsStderr =
  ReadStrategyTestParametricSimple
    "Reports stderr"
    runExitFailure
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    scriptPath :: (IsString a, Semigroup a) => a
    scriptPath = appendScriptsHome "stderr.sh"

    args =
      withNoConfig
        [ "--console-log-command",
          "on",
          scriptPath
        ]

    expected =
      [ withErrorPrefix scriptPath <> "1 second: some stderr",
        withCommandPrefix scriptPath "some output",
        withCommandPrefix scriptPath "some stderr",
        withCommandPrefix scriptPath "more output"
      ]

    -- Really the first one is what we should get if we are ignoring stderr,
    -- but we include all for paranoia (we only want the above 'some stderr').
    unexpected =
      [ withErrorPrefix scriptPath <> "1 second: some output more output",
        withErrorPrefix scriptPath <> "1 second: some output",
        withErrorPrefix scriptPath <> "1 second: more output"
      ]

-- This is a combination "anti-test" and test. The block expectation is the
-- behavior that we are trying to improve, which is verified by the buffer
-- expectation.
slowOutputBroken :: ReadStrategyTestParams
slowOutputBroken =
  ReadStrategyTestSimple
    "Slow output is broken"
    run
    args
    blockAssertions
    blockLineBufferAssertions
  where
    scriptPath :: (IsString a, Semigroup a) => a
    scriptPath = appendScriptsHome "slow_output_broken.sh"

    args =
      withNoConfig
        [ "--console-log-command",
          "on",
          scriptPath
        ]

    blockAssertions results =
      V.verifyExpectedUnexpected results blockExpected blockUnexpected

    blockExpected =
      [ withCommandPrefix scriptPath "first: ",
        withCommandPrefix scriptPath "second"
      ]

    blockUnexpected =
      [ withCommandPrefix scriptPath "first: second"
      ]

    blockLineBufferAssertions results =
      V.verifyExpected results blockLineBufferExpected

    blockLineBufferExpected =
      [ withCommandPrefix scriptPath "first: second"
      ]

formatsFileLogs :: IO TestArgs -> ReadStrategyTestParams
formatsFileLogs testArgs =
  ReadStrategyTestSetup
    "Formats file logs"
    run
    ( \_ -> do
        outFile <- (</> [osp|file-log-formatted.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "on",
                  "--command-log-read-size",
                  "5b", -- intentionally using a low size to test buffering
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpectedN resultsConsole blockConsoleExpected

        resultsFile <- readLogFile outFile
        V.verifyExpectedN resultsFile blockFileExpected
        pure ()
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole bufferConsoleExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile bufferFileExpected
    )
  where
    cmd :: (IsString a, Semigroup a) => a
    cmd = appendScriptsHome "formatting.sh"

    -- NOTE: [Expected line counts]
    --
    -- The below expectations are a bit weird. In general, we expect each line
    -- exactly once. However, we see some lines several times. For instance,
    -- the expectation with no additional output (withCommandPrefix cmd "")
    -- is found on every single line, since it's always a prefix. Hence we
    -- need to expect 22.
    --
    -- It's wasteful to check it multiple times, as we do here, but it makes
    -- the test easier to understand, so we leave it.

    blockConsoleExpected =
      [ (01, withCommandPrefix cmd "Starting"),
        (01, withCommandPrefix cmd "A tit"),
        (01, withCommandPrefix cmd "le"),
        (22, withCommandPrefix cmd ""),
        (01, withCommandPrefix cmd "A hea"),
        (01, withCommandPrefix cmd "der"),
        (22, withCommandPrefix cmd ""),
        (02, withCommandPrefix cmd "-"),
        (01, withCommandPrefix cmd "Runni"),
        (01, withCommandPrefix cmd "ng ta"),
        (01, withCommandPrefix cmd "sk A"),
        (22, withCommandPrefix cmd ""),
        (01, withCommandPrefix cmd "OK"),
        (22, withCommandPrefix cmd ""),
        (01, withCommandPrefix cmd "- R"),
        (01, withCommandPrefix cmd "unnin"),
        (01, withCommandPrefix cmd "g tas"),
        (01, withCommandPrefix cmd "k B"),
        (22, withCommandPrefix cmd ""),
        (01, withCommandPrefix cmd "FAIL"),
        (01, withCommandPrefix cmd "Finis")
      ]

    blockFileExpected =
      [ (1, withCommandPrefix cmd "Starting"),
        (1, withCommandPrefix cmd "A tit"),
        (1, withCommandPrefix cmd "le"),
        (5, withCommandPrefix cmd "  "),
        (1, withCommandPrefix cmd "A hea"),
        (1, withCommandPrefix cmd "der"),
        (7, withCommandPrefix cmd " "),
        (1, withCommandPrefix cmd "   - "),
        (1, withCommandPrefix cmd "Runni"),
        (1, withCommandPrefix cmd "ng ta"),
        (1, withCommandPrefix cmd "sk A "),
        (5, withCommandPrefix cmd "  "),
        (1, withCommandPrefix cmd "OK"),
        (5, withCommandPrefix cmd "  "),
        (1, withCommandPrefix cmd "  - R"),
        (1, withCommandPrefix cmd "unnin"),
        (1, withCommandPrefix cmd "g tas"),
        (1, withCommandPrefix cmd "k B  "),
        (7, withCommandPrefix cmd " "),
        (1, withCommandPrefix cmd "FAIL"),
        (1, withCommandPrefix cmd "Finis")
      ]

    bufferConsoleExpected =
      [ withCommandPrefix cmd "A title",
        withCommandPrefix cmd "A header",
        withCommandPrefix cmd "- Running task A   OK",
        withCommandPrefix cmd "- Running task B   FAIL",
        withCommandPrefix cmd "Finished"
      ]

    bufferFileExpected =
      [ withCommandPrefix cmd "A title",
        withCommandPrefix cmd "  A header",
        withCommandPrefix cmd "    - Running task A   OK",
        withCommandPrefix cmd "    - Running task B   FAIL",
        withCommandPrefix cmd "Finished"
      ]

readStrategyDefaultTests :: IO TestArgs -> TestTree
readStrategyDefaultTests testArgs =
  testGroup
    "read-strategy default tests"
    [ testReadStrategyMultiCmdsFileBlock testArgs,
      testReadStrategyMultiCmdsBuffer,
      testReadStrategyOneCmdFileLogBuffer testArgs
    ]

testReadStrategyMultiCmdsFileBlock :: IO TestArgs -> TestTree
testReadStrategyMultiCmdsFileBlock testArgs = testCase desc $ do
  outFile <- (</> [osp|read-strategy-multi-cmd-block.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--console-log-command",
            "on",
            "--command-log-read-size",
            "2b",
            readStrategyDefaultCmd,
            "sleep 1"
          ]

  consoleResults <- run args
  V.verifyExpected consoleResults expected

  fileResults <- readLogFile outFile
  V.verifyExpected fileResults expected
  where
    desc = "Multiple commands and file logging uses 'block'"
    -- split since read-size = 2
    expected =
      [ withCommandPrefix readStrategyDefaultCmdLog "he",
        withCommandPrefix readStrategyDefaultCmdLog "ll",
        withCommandPrefix readStrategyDefaultCmdLog "o",
        withCommandPrefix readStrategyDefaultCmdLog "by",
        withCommandPrefix readStrategyDefaultCmdLog "e",
        finishedPrefix
      ]

testReadStrategyMultiCmdsBuffer :: TestTree
testReadStrategyMultiCmdsBuffer = testCase desc $ do
  let args =
        withNoConfig
          [ "--console-log-command",
            "on",
            "--command-log-read-size",
            "2b",
            readStrategyDefaultCmd,
            "sleep 1"
          ]

  consoleResults <- run args
  V.verifyExpected consoleResults expected
  where
    desc = "Multiple commands and no file logging uses 'block-line-buffer'"
    -- split since read-size = 2
    expected =
      [ withCommandPrefix readStrategyDefaultCmdLog "hello",
        withCommandPrefix readStrategyDefaultCmdLog "bye",
        finishedPrefix
      ]

testReadStrategyOneCmdFileLogBuffer :: IO TestArgs -> TestTree
testReadStrategyOneCmdFileLogBuffer testArgs = testCase desc $ do
  outFile <- (</> [osp|read-strategy-one-cmd-file-buffer.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--console-log-command",
            "on",
            "--command-log-read-size",
            "2b",
            readStrategyDefaultCmd
          ]

  consoleResults <- run args
  V.verifyExpected consoleResults expected

  fileResults <- readLogFile outFile
  V.verifyExpected fileResults expected
  where
    desc = "One command and file logging uses 'block-line-buffer'"
    -- Only newlines are split since logs w/o newlines are buffered
    expected =
      [ withCommandPrefix readStrategyDefaultCmdLog "hello",
        withCommandPrefix readStrategyDefaultCmdLog "bye",
        finishedPrefix
      ]

readStrategyDefaultCmd :: String
readStrategyDefaultCmd = "printf 'hello\nbye' && sleep 1"

readStrategyDefaultCmdLog :: ExpectedText
readStrategyDefaultCmdLog = "printf 'hello bye' && sleep 1"
