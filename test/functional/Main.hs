-- | Runs functional tests.
module Main (main) where

import Functional.Prelude
import Functional.Readme qualified as Readme
import Functional.Success qualified as Success
import Functional.SuccessCommandLogging qualified as SuccessCommandLogging
import Functional.SuccessFileLogging qualified as SuccessFileLogging
import Functional.SuccessShowKey qualified as SuccessShowKey
import Functional.TestArgs (TestArgs (..))
import Functional.Timeout qualified as Timeout
import Functional.Truncation qualified as Truncation
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Directory qualified as Dir
import Test.Tasty qualified as Tasty

-- | Entry point for functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)
  defaultMain $ Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args = do
  testGroup
    "Functional Tests"
    [ Success.spec args,
      SuccessCommandLogging.spec args,
      SuccessFileLogging.spec args,
      SuccessShowKey.spec args,
      Timeout.spec,
      Truncation.spec,
      Readme.specs args
    ]

setup :: IO TestArgs
setup = do
  rootTmpDir <- (</> "shrun") <$> Dir.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> "test/functional"

  cwd <- (</> "test/functional") <$> Dir.getCurrentDirectory
  let lp = cwd </> "config.toml"

  Dir.createDirectoryIfMissing True workingTmpDir
  pure $
    MkTestArgs
      { rootDir = rootTmpDir,
        tmpDir = workingTmpDir,
        configPath = lp
      }

teardown :: TestArgs -> IO ()
teardown testArgs = do
  let root = testArgs ^. #rootDir
      cwd = testArgs ^. #tmpDir

  void $ tryAny $ do
    removeDirectoryIfExists cwd
    removeDirectoryIfExists $ root </> "test/functional"
    removeDirectoryIfExists $ root </> "test"
    removeDirectoryIfExists root
