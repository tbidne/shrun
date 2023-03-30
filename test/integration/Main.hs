-- | Runs integration tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import Integration.Defaults qualified as Defaults
import Integration.Examples qualified as Examples
import Integration.Failures qualified as Failures
import Integration.Miscellaneous qualified as Miscellaneous
import Integration.Prelude

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

  -- because this is caused in a bracket-style cleanup, we really do not want
  -- this to throw
  void $ tryAny $ do
    -- There are several tests that rely on this log's existence, since it
    -- exists in the 'config' directory, and these tests test that default.
    -- Thus we cannot delete it until everything has finished.
    removeFileIfExists "test/integration/toml/log"
    removeFileIfExists "test/integration/toml/osx/log"

    -- Ideally we want to clean up after ourselves in each test. These are for
    -- insurance.
    removeFileIfExists $ cwd </> "log"
    removeFileIfExists $ cwd </> "large-file-warn"
    removeFileIfExists $ cwd </> "large-file-del"
    removeFileIfExists $ root </> "test/integration"
    removeFileIfExists $ root </> "test"
    removeDirectoryIfExists root
