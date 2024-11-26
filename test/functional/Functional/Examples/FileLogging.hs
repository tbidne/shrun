{-# LANGUAGE QuasiQuotes #-}

module Functional.Examples.FileLogging (tests) where

import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "FileLogging"
    ( [ fileLog args,
        fileLogDeleteOnSuccessFail args
      ]
        ++ multiTestReadStrategy testsParams
    )
  where
    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ fileLogCommandNameTruncN args,
        fileLogDeleteOnSuccess args,
        fileLogLineTruncN args,
        fileLogModeAppend args,
        fileLogModeRename args,
        fileLogModeWrite args,
        fileLogStripControlAll args,
        fileLogStripControlNone args,
        fileLogStripControlSmart args
      ]

fileLog :: IO TestArgs -> TestTree
fileLog testArgs = testCase "Runs file-log example" $ do
  outFile <- (</> [osp|readme-file-out.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "sleep 2",
            "bad",
            "for i in 1 2 3; do echo hi; sleep 1; done"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done",
        finishedPrefix
      ]
    expectedFile =
      expectedConsole
        ++ [ withCommandPrefix "for i in 1 2 3; do echo hi; sleep 1; done" "hi"
           ]

fileLogCommandNameTruncN :: IO TestArgs -> ReadStrategyTestParams
fileLogCommandNameTruncN testArgs =
  ReadStrategyTestParametricSetup
    "Runs --file-log-command-name-trunc 10 example"
    run
    ( do
        outFile <- (</> [osp|readme-file-log-command-name-trunc-out.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-command-name-trunc",
                  "10",
                  "for i in 1 2 3; do echo hi; sleep 1; done"
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expectedConsole

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedConsole =
      [ withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done", -- not truncated
        withFinishedPrefix "3 seconds"
      ]
    expectedFile =
      [ withCommandPrefix "for i i..." "hi",
        withFinishedPrefix "3 seconds"
      ]

fileLogDeleteOnSuccess :: IO TestArgs -> ReadStrategyTestParams
fileLogDeleteOnSuccess testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-delete-on-success example"
    run
    ( do
        outFile <- (</> [osp|del-on-success.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-delete-on-success",
                  "sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expectedConsole

        exists <- doesFileExist outFile

        assertBool "File should not exist" (not exists)
    )
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]

fileLogDeleteOnSuccessFail :: IO TestArgs -> TestTree
fileLogDeleteOnSuccessFail testArgs = testCase "Runs file-log-delete-on-success failure example" $ do
  outFile <- (</> [osp|del-on-success-fail.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-delete-on-success",
            "bad",
            "sleep 2"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  exists <- doesFileExist outFile

  assertBool "File should exist" exists

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogLineTruncN :: IO TestArgs -> ReadStrategyTestParams
fileLogLineTruncN testArgs =
  ReadStrategyTestParametricSetup
    "Runs --file-log-line-trunc 120 example"
    run
    ( do
        outFile <- (</> [osp|line-trunc.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-line-trunc",
                  "120",
                  "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"
                ]

        -- NOTE: We choose 120 so that we get _some_ chars rather than minimal ...,
        -- so the test is more precise.
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "Star...",
        withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "some..."
      ]

-- NOTE: File log mode tests are not configuration.md examples due to
-- simplicity.

fileLogModeAppend :: IO TestArgs -> ReadStrategyTestParams
fileLogModeAppend testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode append"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = tmpDir </> [osp|fileLogModeAppend.log|]
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "append",
                  "sleep 2"
                ]
        pure (args, tmpDir)
    )
    ( \(resultsConsole, tmpDir) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log = tmpDir </> [osp|fileLogModeAppend.log|]

        exists <- doesFileExist log
        assertBool ("File should exist: " <> decodeLenient log) exists
        resultsFile <- readLogFile log
        V.verifyExpected resultsFile expectedConsole

        -- because we are appending 3 lines to the file 3 times
        9 @=? length resultsFile

        let log2 = tmpDir </> [osp|fileLogModeAppend (1).log|]
            log3 = tmpDir </> [osp|fileLogModeAppend (2).log|]

        for_ [log2, log3] $ \badLog -> do
          badExists <- doesFileExist badLog
          assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
    )
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]

fileLogModeRename :: IO TestArgs -> ReadStrategyTestParams
fileLogModeRename testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode rename"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = tmpDir </> [osp|fileLogModeRename.log|]
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "rename",
                  "sleep 2"
                ]
        pure (args, tmpDir)
    )
    ( \(resultsConsole, tmpDir) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log1 = tmpDir </> [osp|fileLogModeRename.log|]
            log2 = tmpDir </> [osp|fileLogModeRename (1).log|]
            log3 = tmpDir </> [osp|fileLogModeRename (2).log|]

        for_ [log1, log2, log3] $ \log -> do
          exists <- doesFileExist log
          assertBool ("File should exist: " <> decodeLenient log) exists

          resultsFile <- readLogFile log
          V.verifyExpected resultsFile expectedFile

        let badLog = tmpDir </> [osp|fileLogModeRename (3).log|]
        exists <- doesFileExist badLog
        assertBool ("File should not exist: " <> decodeLenient badLog) (not exists)
    )
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogModeWrite :: IO TestArgs -> ReadStrategyTestParams
fileLogModeWrite testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode write"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = tmpDir </> [osp|fileLogModeWrite.log|]
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "write",
                  "sleep 2"
                ]
        pure (args, tmpDir)
    )
    ( \(resultsConsole, tmpDir) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log = tmpDir </> [osp|fileLogModeWrite.log|]

        exists <- doesFileExist log
        assertBool ("File should exist: " <> decodeLenient log) exists
        resultsFile <- readLogFile log
        V.verifyExpected resultsFile expectedFile

        let log2 = tmpDir </> [osp|fileLogModeWrite (1).log|]
            log3 = tmpDir </> [osp|fileLogModeWrite (2).log|]

        for_ [log2, log3] $ \badLog -> do
          badExists <- doesFileExist badLog
          assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
    )
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogStripControlAll :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlAll testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control all example"
    run
    ( do
        outFile <- (</> [osp|readme-file-out-strip-control-all.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "all",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 2" " foo  hello  bye "
      ]

fileLogStripControlNone :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlNone testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control none example"
    run
    ( do
        outFile <- (</> [osp|readme-file-out-strip-control-none.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "none",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello \ESC[3D bye"
      ]

fileLogStripControlSmart :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlSmart testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control smart example"
    run
    ( do
        outFile <- (</> [osp|readme-file-out-strip-control-smart.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "smart",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello  bye "
      ]
