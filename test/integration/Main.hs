{-# LANGUAGE QuasiQuotes #-}

-- | Runs integration tests.
module Main (main) where

import Effectful.FileSystem.PathReader.Static (PathReaderStatic)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
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
setup = runEff' $ do
  rootTmpDir <- (</> [osp|shrun|]) <$> PR.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> [osp|test/integration|]

  PW.createDirectoryIfMissing True workingTmpDir
  pure $ MkTestArgs rootTmpDir workingTmpDir

teardown :: TestArgs -> IO ()
teardown testArgs = do
  let root = testArgs ^. #rootTmpDir
      cwd = testArgs ^. #workingTmpDir

  -- because this is called in a bracket-style cleanup, we really do not want
  -- this to throw
  void $ tryAny $ runEff' $ do
    -- There are several tests that rely on this log's existence, since it
    -- exists in the 'config' directory, and these tests test that default.
    -- Thus we cannot delete it until everything has finished.
    removeFileIfExists [osp|test/integration/toml/log|]
    removeFileIfExists [osp|test/integration/toml/osx/log|]

    -- Ideally we want to clean up after ourselves in each test. These are for
    -- insurance.
    removeFileIfExists $ cwd </> [osp|log|]
    removeFileIfExists $ cwd </> [osp|large-file-warn|]
    removeFileIfExists $ cwd </> [osp|large-file-del|]
    removeFileIfExists $ root </> [osp|test/integration|]
    removeFileIfExists $ root </> [osp|test|]
    removeDirectoryIfExists root

runEff' :: Eff [PathWriterStatic, PathReaderStatic, IOE] a -> IO a
runEff' =
  runEff
    . PR.runPathReaderStaticIO
    . PW.runPathWriterStaticIO
