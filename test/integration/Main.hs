{-# LANGUAGE QuasiQuotes #-}

-- | Runs integration tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.FileSystem.PathWriter qualified as Dir
import Integration.Defaults qualified as Defaults
import Integration.Examples qualified as Examples
import Integration.Failures qualified as Failures
import Integration.Miscellaneous qualified as Miscellaneous
import Integration.Prelude
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')

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
          Failures.specs tmpDir,
          Miscellaneous.specs tmpDir
        ]

setup :: IO TestArgs
setup = do
  rootTmpDir <- (</> [osp|shrun|]) <$> Dir.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> [osp|test/integration|]

  Dir.createDirectoryIfMissing True workingTmpDir
  pure $ MkTestArgs rootTmpDir workingTmpDir

teardown :: TestArgs -> IO ()
teardown testArgs = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: "
        <> FsUtils.decodeOsToFpShow (testArgs ^. #rootTmpDir)

    cleanup = do
      let root = testArgs ^. #rootTmpDir
          cwd = testArgs ^. #workingTmpDir

      void $ tryAny $ do
        removeDirectoryIfExists cwd
        removeDirectoryIfExists root
