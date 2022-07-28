-- | Runs integration tests.
module Main (main) where

import Integration.Defaults qualified as Defaults
import Integration.Examples qualified as Examples
import Integration.Failures qualified as Failures
import Integration.Miscellaneous qualified as Miscellaneous
import Integration.Prelude
import System.Directory qualified as Dir

-- | Entry point for integration tests.
main :: IO ()
main = do
  defaultMain (withResource setup teardown tests)
  where
    tests tmpDir =
      testGroup
        "Integration tests"
        [ Defaults.specs tmpDir,
          Examples.specs,
          Failures.specs,
          Miscellaneous.specs tmpDir
        ]

setup :: IO TestArgs
setup = do
  rootTmpDir <- (</> "shrun") <$> Dir.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> "test/integration"

  Dir.createDirectoryIfMissing True workingTmpDir
  pure $ MkTestArgs rootTmpDir workingTmpDir

teardown :: TestArgs -> IO ()
teardown testArgs = do
  let root = testArgs ^. #rootTmpDir
      cwd = testArgs ^. #workingTmpDir

  -- There are several tests that rely on this log's existence, since it
  -- exists in the 'config' directory, and these tests test that default.
  -- Thus we cannot delete it until everything has finished.
  _ <- deleteFileIfExistsNoThrow "test/integration/toml/log"

  -- Ideally we want to clean up after ourselves in each test. These are for
  -- insurance.
  _ <- deleteFileIfExistsNoThrow $ cwd </> "log"
  _ <- deleteFileIfExistsNoThrow $ cwd </> "large-file-warn"
  _ <- deleteFileIfExistsNoThrow $ cwd </> "large-file-del"
  _ <- deleteDirIfExistsNoThrow $ root </> "test/integration"
  _ <- deleteDirIfExistsNoThrow $ root </> "test"
  _ <- deleteDirIfExistsNoThrow root
  pure ()
