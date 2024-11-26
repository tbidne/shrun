{-# LANGUAGE QuasiQuotes #-}

-- | Runs integration tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
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
  rootTmpDir <- (</> [osp|shrun|]) <$> PR.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> [osp|test/integration|]

  PW.createDirectoryIfMissing True workingTmpDir
  pure $ MkTestArgs rootTmpDir workingTmpDir

teardown :: TestArgs -> IO ()
teardown testArgs = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient (testArgs ^. #rootTmpDir)
        <> "'"

    cleanup = do
      let cwd = testArgs ^. #workingTmpDir

      -- NOTE: [Test cleanup]
      --
      -- Don't delete rootTmp because other tests may be using it.
      PW.removeDirectoryRecursiveIfExists_ cwd
