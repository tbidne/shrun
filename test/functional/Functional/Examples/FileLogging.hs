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
    $ [ fileLog args,
        fileLogMulti args,
        fileLogDeleteOnSuccessFail args,
        fileLogCommandNameTruncN args,
        fileLogDeleteOnSuccess args,
        fileLogMultiDeleteOnSuccess args,
        fileLogLineTruncN args,
        fileLogModeAppend args,
        fileLogModeRename args,
        fileLogModeWrite args,
        fileLogMultiAppend args,
        fileLogMultiRename args,
        fileLogMultiWrite args,
        fileLogStripControlAll args,
        fileLogStripControlNone args,
        fileLogStripControlSmart args,
        fileLogDirPathFail args
      ]
    ++ readStrategyTests
  where
    readStrategyTests =
      multiTestReadStrategy
        [ fileLogMultiBuffer args
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
  V.verifyExpectedN resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done",
        finishedPrefix (0, 0, 1, 2)
      ]
    expectedFile =
      zip [1, 1 ..] expectedConsole
        ++ [ (3, withCommandPrefix "for i in 1 2 3; do echo hi; sleep 1; done" "hi")
           ]

fileLogMulti :: IO TestArgs -> TestTree
fileLogMulti testArgs = testCase "Logs commands to each file" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outMain = tmpDir </> [osp|file-out-multi.log|]
      outMainStr = unsafeDecode outMain

      out1 = tmpDir </> [osp|file-out-multi_multi1.log|]
      out2 = tmpDir </> [osp|file-out-multi_multi2.log|]
      out3 = tmpDir </> [osp|file-out-multi_multi3.log|]

      args =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "sleep 2",
            "bad",
            "for i in 1 2 3; do echo hi; sleep 1; done"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  resultsFileMain <- readLogFile outMain
  V.verifyExpectedN resultsFileMain expectedFileMain

  resultsFileMulti1 <- readLogFile out1
  resultsFileMulti2 <- readLogFile out2
  resultsFileMulti3 <- readLogFile out3

  -- combine results since file order is non-deterministic.
  let resultsFileMulti =
        resultsFileMulti1
          ++ resultsFileMulti2
          ++ resultsFileMulti3

  V.verifyExpectedN resultsFileMulti expectedFileMulti
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done",
        finishedPrefix (0, 0, 1, 2)
      ]
    expectedFileMain = zip [1, 1 ..] expectedConsole

    expectedFileMulti =
      [ -- cmd 1
        (1, withCommandPrefix "sleep 2" "Starting..."),
        -- cmd 2
        (1, withCommandPrefix "bad" "Starting..."),
        -- cmd3
        (1, withCommandPrefix "for i in 1 2 3; do echo hi; sleep 1; done" "Starting..."),
        (3, withCommandPrefix "for i in 1 2 3; do echo hi; sleep 1; done" "hi")
      ]

fileLogCommandNameTruncN :: IO TestArgs -> TestTree
fileLogCommandNameTruncN testArgs = testCase "Runs --file-log-command-name-trunc 10 example" $ do
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

  resultsConsole <- run args
  V.verifyExpected resultsConsole expectedConsole

  resultsFile <- readLogFile outFile
  V.verifyExpectedN resultsFile expectedFile
  where
    expectedConsole =
      [ withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done", -- not truncated
        withFinishedPrefix (0, 0, 0, 1) "3 seconds"
      ]
    expectedFile =
      [ (3, withCommandPrefix "for i i..." "hi"),
        (1, withFinishedPrefix (0, 0, 0, 1) "3 seconds")
      ]

fileLogDeleteOnSuccess :: IO TestArgs -> TestTree
fileLogDeleteOnSuccess testArgs = testCase "Runs file-log-delete-on-success example" $ do
  outFile <- (</> [osp|del-on-success.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-delete-on-success",
            "on",
            "sleep 2"
          ]

  resultsConsole <- run args
  V.verifyExpected resultsConsole expectedConsole

  exists <- doesFileExist outFile

  assertBool "File should not exist" (not exists)
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix (0, 0, 0, 1)
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
            "on",
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
        finishedPrefix (0, 0, 1, 1)
      ]
    expectedFile = expectedConsole

fileLogMultiDeleteOnSuccess :: IO TestArgs -> TestTree
fileLogMultiDeleteOnSuccess testArgs = testCase "file-log-multi with --delete-on-success" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outMain = tmpDir </> [osp|del-on-success-multi.log|]
      outMainStr = unsafeDecode outMain

      out1 = tmpDir </> [osp|del-on-success-multi_multi1.log|]
      out2 = tmpDir </> [osp|del-on-success-multi_multi2.log|]

      args =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-delete-on-success",
            "on",
            "bad",
            "sleep 2"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  exists <- doesFileExist outMain
  assertBool "Main file should exist" exists

  resultsFile <- readLogFile outMain
  V.verifyExpected resultsFile expectedFile

  out1Exists <- doesFileExist out1
  out2Exists <- doesFileExist out2
  resultsFileMulti <-
    if out1Exists
      then do
        assertBool "Log multi_2 file should not exist" (not out2Exists)
        readLogFile out1
      else do
        assertBool "Log multi_2 file should exist" out2Exists
        readLogFile out2

  V.verifyExpectedN resultsFileMulti expectedFileMulti
  V.verifyUnexpected resultsFileMulti unexpectedMulti
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix (0, 0, 1, 1)
      ]

    expectedFile = expectedConsole

    expectedFileMulti =
      [ -- cmd 2
        (1, withCommandPrefix "bad" "Starting...")
      ]

    unexpectedMulti =
      [ -- cmd1
        withCommandPrefix "sleep 2" "Starting..."
      ]

fileLogLineTruncN :: IO TestArgs -> TestTree
fileLogLineTruncN testArgs = testCase "Runs --file-log-line-trunc 120 example" $ do
  outFile <- (</> [osp|line-trunc.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      -- NOTE: We choose 120 so that we get _some_ chars rather than minimal ...,
      -- so the test is more precise.
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-line-trunc",
            "120",
            "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"
          ]

  void $ run args
  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "Star...",
        withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "some..."
      ]

-- NOTE: File log mode tests are not configuration.md examples due to
-- simplicity.

-- NOTE: [File Log Mode tests]
--
-- The file log mode tests test the different behaviors of the log file:
--
-- append: logs are appended to the same file.
-- rename: we create multiple log files with sequential names.
-- write: the log file is overwritten.
--
-- In general, this means we create a base log file like
--
--     /tmp/test-name_read-strategy.log
--
-- where read-strategy is used to provide a unique path for each test
-- (e.g. we do not want fileLogModeAppend to use the same path for its
-- buffer and block tests, since the interference will cause failures).
--
-- We then check for existence and content, and then -- depending on the
-- test -- run checks against renamed log files i.e.
--
--     /tmp/test-name_read-strategy (1).log
---
-- For example, in the fileLogModeRename block test, we expect the following to
-- all exist:
--
--     /tmp/fileLogModeRename_block.log
--     /tmp/fileLogModeRename_block (1).log
--     /tmp/fileLogModeRename_block (2).log
--
-- OTOH, for fileLogModeAppend, we expect only
--
--     /tmp/fileLogModeAppend_block.log
--
-- to exist, while the others should __not__ exist. We use the mkLogPath
-- function to make creating these paths a little nicer.

fileLogModeAppend :: IO TestArgs -> TestTree
fileLogModeAppend testArgs = testCase "Runs file-log-mode append" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outFile = mkLogPath tmpDir baseName Nothing
      outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-mode",
            "append",
            "sleep 2"
          ]

  resultsConsole <- run args *> run args *> run args

  V.verifyExpected resultsConsole expectedConsole

  let log = mkLogPath tmpDir baseName Nothing

  exists <- doesFileExist log
  assertBool ("File should exist: " <> decodeLenient log) exists
  resultsFile <- readLogFile log
  V.verifyExpectedN resultsFile expectedFile

  -- because we are appending 3 lines to the file 3 times
  9 @=? length resultsFile

  let log2 = mkLogPath tmpDir baseName (Just 1)
      log3 = mkLogPath tmpDir baseName (Just 2)

  for_ [log2, log3] $ \badLog -> do
    badExists <- doesFileExist badLog
    assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
  where
    baseName = [osp|fileLogModeAppend_|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix (0, 0, 0, 1)
      ]
    expectedFile = zip [3, 3] expectedConsole

fileLogModeRename :: IO TestArgs -> TestTree
fileLogModeRename testArgs = testCase "Runs file-log-mode rename" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outFile = mkLogPath tmpDir baseName Nothing
      outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-mode",
            "rename",
            "sleep 2"
          ]

  resultsConsole <- run args *> run args *> run args

  V.verifyExpected resultsConsole expectedConsole

  let log1 = mkLogPath tmpDir baseName Nothing
      log2 = mkLogPath tmpDir baseName (Just 1)
      log3 = mkLogPath tmpDir baseName (Just 2)

  for_ [log1, log2, log3] $ \log -> do
    exists <- doesFileExist log
    assertBool ("File should exist: " <> decodeLenient log) exists

    resultsFile <- readLogFile log
    V.verifyExpected resultsFile expectedFile

    3 @=? length resultsFile

  let badLog = mkLogPath tmpDir baseName (Just 3)
  exists <- doesFileExist badLog
  assertBool ("File should not exist: " <> decodeLenient badLog) (not exists)
  where
    baseName = [osp|fileLogModeRename|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix (0, 0, 0, 1)
      ]
    expectedFile = expectedConsole

fileLogModeWrite :: IO TestArgs -> TestTree
fileLogModeWrite testArgs = testCase "Runs file-log-mode write" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outFile = mkLogPath tmpDir baseName Nothing
      outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-mode",
            "write",
            "sleep 2"
          ]

  resultsConsole <- run args *> run args *> run args

  V.verifyExpected resultsConsole expectedConsole

  let log = mkLogPath tmpDir baseName Nothing

  exists <- doesFileExist log
  assertBool ("File should exist: " <> decodeLenient log) exists
  resultsFile <- readLogFile log
  V.verifyExpected resultsFile expectedFile

  3 @=? length resultsFile

  let log2 = mkLogPath tmpDir baseName (Just 1)
      log3 = mkLogPath tmpDir baseName (Just 2)

  for_ [log2, log3] $ \badLog -> do
    badExists <- doesFileExist badLog
    assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
  where
    baseName = [osp|fileLogModeWrite|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix (0, 0, 0, 1)
      ]
    expectedFile = expectedConsole

fileLogMultiAppend :: IO TestArgs -> TestTree
fileLogMultiAppend testArgs = testCase "Multi respects mode (append)" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outMain = tmpDir </> [osp|file-out-multi-append.log|]
      outMainStr = unsafeDecode outMain

      out1 = tmpDir </> [osp|file-out-multi-append_multi1.log|]
      out2 = tmpDir </> [osp|file-out-multi-append_multi2.log|]

      args1 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "append",
            "echo A && sleep 1",
            "echo B && sleep 1"
          ]

      args2 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "append",
            "echo C && sleep 1",
            "echo D && sleep 1"
          ]

  -- 1 ASSERTS
  resultsConsole1 <- run args1
  V.verifyExpected resultsConsole1 expectedConsole1

  resultsFileMain1 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain1 expectedFileMain1

  -- combine results since file order is non-deterministic.
  resultsFileMulti1 <- liftA2 (++) (readLogFile out1) (readLogFile out2)
  V.verifyExpectedN resultsFileMulti1 expectedFileMulti1

  -- 2 ASSERTS
  resultsConsole2 <- run args2
  V.verifyExpected resultsConsole2 expectedConsole2

  resultsFileMain2 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain2 expectedFileMain2

  resultsFileMulti2 <- liftA2 (++) (readLogFile out1) (readLogFile out2)
  V.verifyExpectedN resultsFileMulti2 expectedFileMulti2
  where
    expectedConsole1 =
      [ withSuccessPrefix "echo A && sleep 1",
        withSuccessPrefix "echo B && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain1 = zip [1, 1 ..] expectedConsole1

    expectedFileMulti1 =
      [ -- cmd 1
        (1, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo A && sleep 1" "A"),
        -- cmd 2
        (1, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo B && sleep 1" "B")
      ]

    expectedConsole2 =
      [ withSuccessPrefix "echo C && sleep 1",
        withSuccessPrefix "echo D && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain2 =
      [ -- Old logs
        (1, withSuccessPrefix "echo A && sleep 1"),
        (1, withSuccessPrefix "echo B && sleep 1"),
        -- New logs
        (1, withSuccessPrefix "echo C && sleep 1"),
        (1, withSuccessPrefix "echo D && sleep 1"),
        -- Both statuses
        (2, finishedPrefix (0, 0, 0, 2))
      ]

    expectedFileMulti2 =
      [ -- Old logs
        (1, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo A && sleep 1" "A"),
        (1, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo B && sleep 1" "B"),
        -- New logs
        (1, withCommandPrefix "echo C && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo C && sleep 1" "C"),
        (1, withCommandPrefix "echo D && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo D && sleep 1" "D")
      ]

fileLogMultiRename :: IO TestArgs -> TestTree
fileLogMultiRename testArgs = testCase "Multi respects mode (rename)" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outMain = tmpDir </> [osp|file-out-multi-rename.log|]
      outMainStr = unsafeDecode outMain

      out1 = tmpDir </> [osp|file-out-multi-rename_multi1.log|]
      out2 = tmpDir </> [osp|file-out-multi-rename_multi2.log|]
      out3 = tmpDir </> [osp|file-out-multi-rename_multi3.log|]
      out4 = tmpDir </> [osp|file-out-multi-rename_multi4.log|]

      args1 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "rename",
            "echo A && sleep 1",
            "echo B && sleep 1"
          ]

      args2 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "rename",
            "echo C && sleep 1",
            "echo D && sleep 1"
          ]

  -- 1 ASSERTS
  resultsConsole1 <- run args1
  V.verifyExpected resultsConsole1 expectedConsole1

  resultsFileMain1 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain1 expectedFileMain1

  -- combine results since file order is non-deterministic.
  resultsFileMulti1 <- liftA2 (++) (readLogFile out1) (readLogFile out2)
  V.verifyExpectedN resultsFileMulti1 expectedFileMulti1

  -- Remove main log file from run 1. Why? After the first run, we will have
  -- created:
  --
  --   - file-out-multi-rename.log
  --   - file-out-multi-rename_multi1.log
  --   - file-out-multi-rename_multi2.log
  --
  -- If we do not remove file-out-multi-rename.log, the renamer will create
  --
  --   - file-out-multi-rename (1).log
  --
  -- And the multi logs will be based off /this/ file:
  --
  --   - file-out-multi-rename (1)_multi1.log
  --   - file-out-multi-rename (1)_multi2.log
  --
  -- But we want to test multi + renamer /not/ the normal renamer, hence we
  -- need the multi names to clash. The easiest way is to simply delete
  -- the main log file, so that shrun happily uses the same name, and
  -- the multi logs collide.
  removeFileIfExists_ outMain

  -- 2 ASSERTS
  resultsConsole2 <- run args2
  V.verifyExpected resultsConsole2 expectedConsole2

  resultsFileMain2 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain2 expectedFileMain2

  resultsFileMulti2 <- liftA2 (++) (readLogFile out3) (readLogFile out4)
  V.verifyExpectedN resultsFileMulti2 expectedFileMulti2
  where
    expectedConsole1 =
      [ withSuccessPrefix "echo A && sleep 1",
        withSuccessPrefix "echo B && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain1 = zip [1, 1 ..] expectedConsole1

    expectedFileMulti1 =
      [ -- cmd 1
        (1, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo A && sleep 1" "A"),
        -- cmd 2
        (1, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo B && sleep 1" "B")
      ]

    expectedConsole2 =
      [ withSuccessPrefix "echo C && sleep 1",
        withSuccessPrefix "echo D && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain2 =
      [ -- Old logs
        (0, withSuccessPrefix "echo A && sleep 1"),
        (0, withSuccessPrefix "echo B && sleep 1"),
        -- New logs
        (1, withSuccessPrefix "echo C && sleep 1"),
        (1, withSuccessPrefix "echo D && sleep 1"),
        -- 1 statuses
        (1, finishedPrefix (0, 0, 0, 2))
      ]

    expectedFileMulti2 =
      [ -- Old logs
        (0, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (0, withCommandPrefix "echo A && sleep 1" "A"),
        (0, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (0, withCommandPrefix "echo B && sleep 1" "B"),
        -- New logs
        (1, withCommandPrefix "echo C && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo C && sleep 1" "C"),
        (1, withCommandPrefix "echo D && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo D && sleep 1" "D")
      ]

fileLogMultiWrite :: IO TestArgs -> TestTree
fileLogMultiWrite testArgs = testCase "Multi respects mode (write)" $ do
  tmpDir <- view #tmpDir <$> testArgs
  let outMain = tmpDir </> [osp|file-out-multi-write.log|]
      outMainStr = unsafeDecode outMain

      out1 = tmpDir </> [osp|file-out-multi-write_multi1.log|]
      out2 = tmpDir </> [osp|file-out-multi-write_multi2.log|]

      args1 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "write",
            "echo A && sleep 1",
            "echo B && sleep 1"
          ]

      args2 =
        withNoConfig
          [ "--file-log",
            outMainStr,
            "--file-log-multi",
            "on",
            "--file-log-mode",
            "write",
            "echo C && sleep 1",
            "echo D && sleep 1"
          ]

  -- 1 ASSERTS
  resultsConsole1 <- run args1
  V.verifyExpected resultsConsole1 expectedConsole1

  resultsFileMain1 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain1 expectedFileMain1

  -- combine results since file order is non-deterministic.
  resultsFileMulti1 <- liftA2 (++) (readLogFile out1) (readLogFile out2)
  V.verifyExpectedN resultsFileMulti1 expectedFileMulti1

  -- 2 ASSERTS
  resultsConsole2 <- run args2
  V.verifyExpected resultsConsole2 expectedConsole2

  resultsFileMain2 <- readLogFile outMain
  V.verifyExpectedN resultsFileMain2 expectedFileMain2

  resultsFileMulti2 <- liftA2 (++) (readLogFile out1) (readLogFile out2)
  V.verifyExpectedN resultsFileMulti2 expectedFileMulti2
  where
    expectedConsole1 =
      [ withSuccessPrefix "echo A && sleep 1",
        withSuccessPrefix "echo B && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain1 = zip [1, 1 ..] expectedConsole1

    expectedFileMulti1 =
      [ -- cmd 1
        (1, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo A && sleep 1" "A"),
        -- cmd 2
        (1, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo B && sleep 1" "B")
      ]

    expectedConsole2 =
      [ withSuccessPrefix "echo C && sleep 1",
        withSuccessPrefix "echo D && sleep 1",
        finishedPrefix (0, 0, 0, 2)
      ]
    expectedFileMain2 =
      [ -- Old logs
        (0, withSuccessPrefix "echo A && sleep 1"),
        (0, withSuccessPrefix "echo B && sleep 1"),
        -- New logs
        (1, withSuccessPrefix "echo C && sleep 1"),
        (1, withSuccessPrefix "echo D && sleep 1"),
        -- 1 statuses
        (1, finishedPrefix (0, 0, 0, 2))
      ]

    expectedFileMulti2 =
      [ -- Old logs
        (0, withCommandPrefix "echo A && sleep 1" "Starting..."),
        (0, withCommandPrefix "echo A && sleep 1" "A"),
        (0, withCommandPrefix "echo B && sleep 1" "Starting..."),
        (0, withCommandPrefix "echo B && sleep 1" "B"),
        -- New logs
        (1, withCommandPrefix "echo C && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo C && sleep 1" "C"),
        (1, withCommandPrefix "echo D && sleep 1" "Starting..."),
        (1, withCommandPrefix "echo D && sleep 1" "D")
      ]

fileLogStripControlAll :: IO TestArgs -> TestTree
fileLogStripControlAll testArgs = testCase "Runs file-log strip-control all example" $ do
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

  void $ run args
  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 2" " foo  hello  bye "
      ]

fileLogStripControlNone :: IO TestArgs -> TestTree
fileLogStripControlNone testArgs = testCase "Runs file-log strip-control none example" $ do
  outFile <- (</> [osp|readme-file-out-strip-control-none.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-strip-control",
            "off",
            "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
          ]

  void $ run args
  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello \ESC[3D bye"
      ]

fileLogStripControlSmart :: IO TestArgs -> TestTree
fileLogStripControlSmart testArgs = testCase "Runs file-log strip-control smart example" $ do
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

  void $ run args
  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello  bye "
      ]

-- | Not an example test, but placed here w/ other file tests.
fileLogDirPathFail :: IO TestArgs -> TestTree
fileLogDirPathFail _testArgs = testCase desc $ do
  let args =
        withNoConfig
          [ "--file-log",
            "documentation",
            "sleep 2"
          ]

  (resultsConsole, ex) <- runException @StringException args
  V.verifyExpected resultsConsole []

  -- Checking we didn't do anything here...
  dirExists <- doesDirectoryExist [osp|documentation|]
  assertBool "Dir should exist" dirExists
  for_ expectedFiles $ \p -> do
    let fp = [osp|documentation|] </> p
    fileExists <- doesFileExist fp
    assertBool ("File " ++ show fp ++ " should exist") fileExists

  expected @=? displayException ex
  where
    desc = "file-log with extant directory fails"

    expectedFiles =
      [ [osp|configuration.md|],
        [osp|development.md|],
        [osp|faq.md|]
      ]

    expected =
      mconcat
        [ "Requested log file 'documentation' already exists and has ",
          "invalid type: directory."
        ]

-- Copy of formatsFileLogs test in Miscellaneous, but with --file-log-multi.
fileLogMultiBuffer :: IO TestArgs -> ReadStrategyTestParams
fileLogMultiBuffer testArgs =
  ReadStrategyTestSetup
    "Multi log with buffer read-strategy"
    run
    ( \pfx -> do
        tmpDir <- view #tmpDir <$> testArgs
        let outMain = tmpDir </> pfx <> [osp|_file-log-multi-buffer.log|]
            out1 = tmpDir </> pfx <> [osp|_file-log-multi-buffer_multi1.log|]
            out2 = tmpDir </> pfx <> [osp|_file-log-multi-buffer_multi2.log|]

        let outMainStr = unsafeDecode outMain
            args =
              withNoConfig
                [ "--file-log",
                  outMainStr,
                  "--file-log-multi",
                  "on",
                  "--console-log-command",
                  "on",
                  "--command-log-read-size",
                  "5b",
                  cmd1,
                  cmd2
                ]

        pure (args, (outMain, out1, out2))
    )
    ( \(resultsConsole, (outMain, out1, out2)) -> do
        V.verifyExpectedN resultsConsole blockConsoleExpected

        resultsFileMain <- readLogFile outMain
        V.verifyExpected resultsFileMain blockFileExpected

        resultsFileMulti1 <- readLogFile out1
        resultsFileMulti2 <- readLogFile out2

        let resultsFileMulti = resultsFileMulti1 ++ resultsFileMulti2

        V.verifyExpectedN resultsFileMulti blockFileMultiExpected
    )
    ( \(resultsConsole, (outMain, out1, out2)) -> do
        V.verifyExpected resultsConsole bufferConsoleExpected

        resultsFileMain <- readLogFile outMain
        V.verifyExpected resultsFileMain bufferFileExpected

        resultsFileMulti1 <- readLogFile out1
        resultsFileMulti2 <- readLogFile out2

        let resultsFileMulti = resultsFileMulti1 ++ resultsFileMulti2

        V.verifyExpected resultsFileMulti bufferFileMultiExpected
    )
  where
    cmd1 :: (IsString a, Semigroup a) => a
    cmd1 = appendScriptsHome "formatting.sh"

    cmd2 :: (IsString a) => a
    cmd2 = "sleep 1 && echo '   hi' && sleep 2"

    blockConsoleExpected =
      [ (01, withCommandPrefix cmd1 "Starting"),
        (01, withCommandPrefix cmd1 "A tit"),
        (01, withCommandPrefix cmd1 "le"),
        (22, withCommandPrefix cmd1 ""),
        (01, withCommandPrefix cmd1 "A hea"),
        (01, withCommandPrefix cmd1 "der"),
        (22, withCommandPrefix cmd1 ""),
        (02, withCommandPrefix cmd1 "-"),
        (01, withCommandPrefix cmd1 "Runni"),
        (01, withCommandPrefix cmd1 "ng ta"),
        (01, withCommandPrefix cmd1 "sk A"),
        (22, withCommandPrefix cmd1 ""),
        (01, withCommandPrefix cmd1 "OK"),
        (22, withCommandPrefix cmd1 ""),
        (01, withCommandPrefix cmd1 "- R"),
        (01, withCommandPrefix cmd1 "unnin"),
        (01, withCommandPrefix cmd1 "g tas"),
        (01, withCommandPrefix cmd1 "k B"),
        (22, withCommandPrefix cmd1 ""),
        (01, withCommandPrefix cmd1 "FAIL"),
        (01, withCommandPrefix cmd1 "Finis"),
        (01, withCommandPrefix cmd2 "hi")
      ]

    blockFileExpected =
      [ withSuccessPrefix cmd1,
        withSuccessPrefix cmd2,
        finishedPrefix (0, 0, 0, 2)
      ]

    blockFileMultiExpected =
      [ (1, withCommandPrefix cmd1 "Starting"),
        (1, withCommandPrefix cmd1 "A tit"),
        (1, withCommandPrefix cmd1 "le"),
        (5, withCommandPrefix cmd1 "  "),
        (1, withCommandPrefix cmd1 "A hea"),
        (1, withCommandPrefix cmd1 "der"),
        (7, withCommandPrefix cmd1 " "),
        (1, withCommandPrefix cmd1 "   - "),
        (1, withCommandPrefix cmd1 "Runni"),
        (1, withCommandPrefix cmd1 "ng ta"),
        (1, withCommandPrefix cmd1 "sk A "),
        (5, withCommandPrefix cmd1 "  "),
        (1, withCommandPrefix cmd1 "OK"),
        (5, withCommandPrefix cmd1 "  "),
        (1, withCommandPrefix cmd1 "  - R"),
        (1, withCommandPrefix cmd1 "unnin"),
        (1, withCommandPrefix cmd1 "g tas"),
        (1, withCommandPrefix cmd1 "k B  "),
        (7, withCommandPrefix cmd1 " "),
        (1, withCommandPrefix cmd1 "FAIL"),
        (1, withCommandPrefix cmd1 "Finis"),
        (1, withCommandPrefix cmd2 "   hi")
      ]

    bufferConsoleExpected =
      [ withCommandPrefix cmd1 "A title",
        withCommandPrefix cmd1 "A header",
        withCommandPrefix cmd1 "- Running task A   OK",
        withCommandPrefix cmd1 "- Running task B   FAIL",
        withCommandPrefix cmd1 "Finished",
        withCommandPrefix cmd2 "hi"
      ]

    bufferFileExpected = blockFileExpected

    bufferFileMultiExpected =
      [ withCommandPrefix cmd1 "A title",
        withCommandPrefix cmd1 "  A header",
        withCommandPrefix cmd1 "    - Running task A   OK",
        withCommandPrefix cmd1 "    - Running task B   FAIL",
        withCommandPrefix cmd1 "Finished",
        withCommandPrefix cmd2 "   hi"
      ]

-- | mkLogPath is used to make log paths, based on the base path
-- (used for uniqueness) and sequential number (for file-log-mode rename)
--
-- Examples:
--
-- @
--   -- mkLogPath /tmp fileLogModeAppend Nothing
--   "\/tmp\/fileLogModeAppend.log"
--
--   -- mkLogPath /tmp fileLogModeAppend (Just 1)
--   "\/tmp\/fileLogModeAppend (1).log"
--
--   -- mkLogPath /tmp fileLogModeAppend (Just 2)
--   "\/tmp\/fileLogModeAppend (2).log"
-- @
--
-- See NOTE: [File Log Mode tests] for the motivation.
mkLogPath ::
  -- | Temp directory
  OsPath ->
  -- | base name
  OsPath ->
  -- | Sequence file number, if applicable
  Maybe Int ->
  -- | Combined name
  OsPath
mkLogPath tmpDir base mSEqNum =
  tmpDir </> base <> suffix
  where
    suffix = case mSEqNum of
      Nothing -> [osp|.log|]
      Just i -> [osp| (|] <> unsafeEncode (show i) <> [osp|).log|]
