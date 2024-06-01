{-# LANGUAGE QuasiQuotes #-}

module Functional.Examples.CommandLogging (tests) where

import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier (ExpectedText)
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: IO TestArgs -> TestTree
tests testArgs =
  testGroup
    "CommandLogging"
    (multiTestReadStrategy testsParams)
  where
    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ readSizeDefault,
        readSize,
        bufferLengthSplit testArgs,
        bufferLengthUnsplit testArgs,
        bufferTimeoutSplit testArgs,
        bufferTimeoutUnsplit testArgs
      ]

readSizeDefault :: ReadStrategyTestParams
readSizeDefault =
  ReadStrategyTestSimple
    "Default --read-size splits 16,000"
    run
    args
    blockAssertions
    blockLineBufferAssertions
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--console-log-command-name-trunc",
          "5",
          "--command-log-buffer-length",
          "20_000", -- buffer length needs to be > read-size to not interfere
          cmd
        ]
    cmd = "sleep 1 ; echo " ++ commandLog ++ "b; sleep 1"

    blockAssertions = (`V.verifyExpected` blockExpected)

    blockExpected =
      [ withCommandPrefix "sl..." cmdExpected,
        withCommandPrefix "sl..." "b"
      ]

    blockLineBufferAssertions = (`V.verifyExpected` blockLineBufferExpected)

    blockLineBufferExpected =
      [ withCommandPrefix "sl..." (cmdExpected <> "b")
      ]

cmdExpected :: ExpectedText
cmdExpected = V.MkExpectedText . T.pack $ commandLog

commandLog :: String
commandLog = replicate 16_000 'a'

readSize :: ReadStrategyTestParams
readSize =
  ReadStrategyTestSimple
    "Runs --command-log-read-size example"
    run
    args
    blockAssertions
    blockLineBufferAssertions
  where
    args =
      withNoConfig
        [ "--console-log-command",
          "--command-log-read-size",
          "5b",
          "--command-log-poll-interval",
          "1_000_000",
          cmd
        ]
    cmd :: (IsString a) => a
    cmd = "echo abcdef && sleep 3"

    blockAssertions = (`V.verifyExpected` blockExpected)

    blockExpected =
      [ withCommandPrefix cmd "abcde",
        withCommandPrefix cmd "f"
      ]

    blockLineBufferAssertions = (`V.verifyExpected` blockLineBufferExpected)

    blockLineBufferExpected =
      [ withCommandPrefix cmd "abcdef"
      ]

bufferLengthSplit :: IO TestArgs -> ReadStrategyTestParams
bufferLengthSplit testArgs =
  ReadStrategyTestParametricSetup
    "Runs --command-log-buffer-length split example"
    run
    ( do
        outFile <- (</> [osp|buffer-length-split.log|]) . view #tmpDir <$> testArgs
        let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "--command-log-buffer-length",
                  "1",
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expected
    )
  where
    cmd :: (IsString a) => a
    cmd = "printf hi && sleep 1 && printf b && sleep 1"

    expected =
      [ withCommandPrefix cmd "hi",
        withCommandPrefix cmd "b"
      ]

bufferLengthUnsplit :: IO TestArgs -> ReadStrategyTestParams
bufferLengthUnsplit testArgs =
  ReadStrategyTestSetup
    "Runs --command-log-buffer-length unsplit example"
    run
    ( do
        outFile <- (</> [osp|buffer-length-unsplit.log|]) . view #tmpDir <$> testArgs
        let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "--command-log-buffer-length",
                  "3",
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole blockExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile blockExpected
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole blockLineBufferExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile blockLineBufferExpected
    )
  where
    cmd :: (IsString a) => a
    cmd = "printf hi && sleep 1 && printf b && sleep 1"

    blockExpected =
      [ withCommandPrefix cmd "hi",
        withCommandPrefix cmd "b"
      ]

    blockLineBufferExpected =
      [ withCommandPrefix cmd "hib"
      ]

bufferTimeoutSplit :: IO TestArgs -> ReadStrategyTestParams
bufferTimeoutSplit testArgs =
  ReadStrategyTestParametricSetup
    "Runs --command-log-buffer-timeout split example"
    run
    ( do
        outFile <- (</> [osp|buffer-timeout-split.log|]) . view #tmpDir <$> testArgs
        let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "--command-log-buffer-timeout",
                  "1",
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expected
    )
  where
    cmd :: (IsString a) => a
    cmd = "printf hi && sleep 3 && printf b && sleep 1"

    expected =
      [ withCommandPrefix cmd "hi",
        withCommandPrefix cmd "b"
      ]

bufferTimeoutUnsplit :: IO TestArgs -> ReadStrategyTestParams
bufferTimeoutUnsplit testArgs =
  ReadStrategyTestSetup
    "Runs --command-log-buffer-timeout unsplit example"
    run
    ( do
        outFile <- (</> [osp|buffer-timeout-unsplit.log|]) . view #tmpDir <$> testArgs
        let outFileStr = FsUtils.unsafeDecodeOsToFp outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--console-log-command",
                  "--command-log-buffer-timeout",
                  "2",
                  cmd
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole blockExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile blockExpected
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole bufferExpected

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile bufferExpected
    )
  where
    cmd :: (IsString a) => a
    cmd = "printf hi && sleep 2 && printf b && sleep 1"

    blockExpected =
      [ withCommandPrefix cmd "hi",
        withCommandPrefix cmd "b"
      ]

    bufferExpected =
      [ withCommandPrefix cmd "hib"
      ]
