{-# LANGUAGE QuasiQuotes #-}

-- | Runs functional tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import Effects.FileSystem.Utils qualified as FsUtils
import Functional.Buffering qualified as Buffering
import Functional.Examples qualified as Examples
import Functional.Miscellaneous qualified as Miscellaneous
import Functional.Notify qualified as Notify
import Functional.Prelude
import Functional.Success qualified as Success
import Functional.SuccessCommandLogging qualified as SuccessCommandLogging
import Functional.SuccessFileLogging qualified as SuccessFileLogging
import Functional.SuccessShowKey qualified as SuccessShowKey
import Functional.TestArgs (TestArgs (MkTestArgs, configPath, rootDir, tmpDir))
import Functional.Timeout qualified as Timeout
import Functional.Truncation qualified as Truncation
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Tasty qualified as Tasty

-- | Entry point for functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  defaultMain $ Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args = do
  testGroup
    "Functional Tests"
    [ Examples.specs args,
      Buffering.specs,
      Miscellaneous.specs,
      Notify.specs,
      Success.spec args,
      SuccessCommandLogging.spec,
      SuccessFileLogging.spec args,
      SuccessShowKey.spec args,
      Timeout.spec,
      Truncation.spec
    ]

setup :: IO TestArgs
setup = do
  rootTmpDir <- (</> [osp|shrun|]) <$> Dir.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> [osp|test/functional|]

  cwd <- (</> [osp|test/functional|]) <$> Dir.getCurrentDirectory
  let lp = cwd </> [osp|config.toml|]

  Dir.createDirectoryIfMissing True workingTmpDir
  pure
    $ MkTestArgs
      { rootDir = rootTmpDir,
        tmpDir = workingTmpDir,
        configPath = lp
      }

teardown :: TestArgs -> IO ()
teardown testArgs = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = do
      let root = testArgs ^. #rootDir
          cwd = testArgs ^. #tmpDir

      void $ tryAny $ do
        removeDirectoryIfExists cwd
        removeDirectoryIfExists $ root </> [osp|test/functional|]
        removeDirectoryIfExists $ root </> [osp|test|]
        removeDirectoryIfExists root

    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: "
        <> FsUtils.decodeOsToFpShow (testArgs ^. #tmpDir)
