{-# LANGUAGE QuasiQuotes #-}

-- | Misc tests
module Functional.Miscellaneous (specs) where

import Effects.FileSystem.Utils qualified as FsUtils
import Functional.Prelude
import Functional.TestArgs (TestArgs)
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
        spaceErrorLogs,
        stripControlAlwaysCmdNames,
        reportsStderr,
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

spaceErrorLogs :: ReadStrategyTestParams
spaceErrorLogs =
  ReadStrategyTestParametricSimple
    "Error Log with newlines is spaced"
    runExitFailure
    args
    (\results -> V.verifyExpectedUnexpected results expected unexpected)
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "sleep 1 && echo 'abc\n  def' && sleep 1 && exit 1"
        ]

    -- Verifying final 'abc\n  def' log is translated to 'abc  def' in the final
    -- error msg. Breakdown:
    --
    -- - In command names, newlines are converted to spaces, so 'abc\n  def'
    --   'abc   def'.
    --
    -- - In command logs, newlines are split across separate logs. Hence
    --   'abc' and '  def'.
    --
    -- - In the final error message, it appears newlines are just stripped?
    --   Should probably be the same as command names.
    expected =
      [ withErrorPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" <> "2 seconds: abc   def",
        withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "abc",
        withCommandPrefix "sleep 1 && echo 'abc   def' && sleep 1 && exit 1" "def"
      ]
    unexpected =
      [ withErrorPrefix "sleep 1 && echo 'abc def' && sleep 1 && exit 1" <> "2 seconds: abcdef"
      ]

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
          "--console-log-strip-control",
          "none",
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
    scriptPath :: (IsString a) => a
    scriptPath = "./test/functional/Functional/stderr.sh"

    args =
      withNoConfig
        [ "--console-log-command",
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
    scriptPath :: (IsString a) => a
    scriptPath = "./test/functional/Functional/slow_output_broken.sh"

    args =
      withNoConfig
        [ "--console-log-command",
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
    ( do
        outFile <- (</> [osp|file-log-formatted.log|]) . view #tmpDir <$> testArgs
        let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "--command-log-read-size",
                  "5b", -- intentionally using a low size to test buffering
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole blockConsoleExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile blockFileExpected
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole bufferConsoleExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile bufferFileExpected
    )
  where
    cmd :: (IsString a) => a
    cmd = "test/functional/Functional/formatting.sh"

    blockConsoleExpected =
      [ withCommandPrefix cmd "Starting",
        withCommandPrefix cmd "A tit",
        withCommandPrefix cmd "le",
        withCommandPrefix cmd "",
        withCommandPrefix cmd "A hea",
        withCommandPrefix cmd "der",
        withCommandPrefix cmd "",
        withCommandPrefix cmd "-",
        withCommandPrefix cmd "Runni",
        withCommandPrefix cmd "ng ta",
        withCommandPrefix cmd "sk A",
        withCommandPrefix cmd "",
        withCommandPrefix cmd "OK",
        withCommandPrefix cmd "",
        withCommandPrefix cmd "- R",
        withCommandPrefix cmd "unnin",
        withCommandPrefix cmd "g tas",
        withCommandPrefix cmd "k B",
        withCommandPrefix cmd "",
        withCommandPrefix cmd "FAIL",
        withCommandPrefix cmd "Finis"
      ]

    blockFileExpected =
      [ withCommandPrefix cmd "Starting",
        withCommandPrefix cmd "A tit",
        withCommandPrefix cmd "le",
        withCommandPrefix cmd "  ",
        withCommandPrefix cmd "A hea",
        withCommandPrefix cmd "der",
        withCommandPrefix cmd " ",
        withCommandPrefix cmd "   - ",
        withCommandPrefix cmd "Runni",
        withCommandPrefix cmd "ng ta",
        withCommandPrefix cmd "sk A ",
        withCommandPrefix cmd "  ",
        withCommandPrefix cmd "OK",
        withCommandPrefix cmd "  ",
        withCommandPrefix cmd "  - R",
        withCommandPrefix cmd "unnin",
        withCommandPrefix cmd "g tas",
        withCommandPrefix cmd "k B  ",
        withCommandPrefix cmd " ",
        withCommandPrefix cmd "FAIL",
        withCommandPrefix cmd "Finis"
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
    [ testReadStrategyMultiCmdsBlock testArgs,
      testReadStrategyNoFileLogBlock,
      testReadStrategyOneCmdFileLogBuffer testArgs
    ]

testReadStrategyMultiCmdsBlock :: IO TestArgs -> TestTree
testReadStrategyMultiCmdsBlock testArgs = testCase "Multiple commands uses 'block'" $ do
  outFile <- (</> [osp|read-strategy-multi-cmd-block.log|]) . view #tmpDir <$> testArgs
  let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--console-log-command",
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
    -- split since read-size = 2
    expected =
      [ withCommandPrefix readStrategyDefaultCmdLog "he",
        withCommandPrefix readStrategyDefaultCmdLog "ll",
        withCommandPrefix readStrategyDefaultCmdLog "o",
        withCommandPrefix readStrategyDefaultCmdLog "by",
        withCommandPrefix readStrategyDefaultCmdLog "e",
        finishedPrefix
      ]

testReadStrategyNoFileLogBlock :: TestTree
testReadStrategyNoFileLogBlock = testCase "No file logging uses 'block'" $ do
  let args =
        withNoConfig
          [ "--console-log-command",
            "--command-log-read-size",
            "2b",
            readStrategyDefaultCmd
          ]

  consoleResults <- run args
  V.verifyExpected consoleResults expected
  where
    -- split since read-size = 2
    expected =
      [ withCommandPrefix readStrategyDefaultCmdLog "he",
        withCommandPrefix readStrategyDefaultCmdLog "ll",
        withCommandPrefix readStrategyDefaultCmdLog "o",
        withCommandPrefix readStrategyDefaultCmdLog "by",
        withCommandPrefix readStrategyDefaultCmdLog "e",
        finishedPrefix
      ]

testReadStrategyOneCmdFileLogBuffer :: IO TestArgs -> TestTree
testReadStrategyOneCmdFileLogBuffer testArgs = testCase desc $ do
  outFile <- (</> [osp|read-strategy-one-cmd-file-buffer.log|]) . view #tmpDir <$> testArgs
  let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--console-log-command",
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
