{-# LANGUAGE QuasiQuotes #-}

-- | Runs functional tests.
module Main (main) where

import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Functional.Miscellaneous qualified as Miscellaneous
import Functional.Notify qualified as Notify
import Functional.Prelude
import Functional.Readme qualified as Readme
import Functional.Success qualified as Success
import Functional.SuccessCommandLogging qualified as SuccessCommandLogging
import Functional.SuccessFileLogging qualified as SuccessFileLogging
import Functional.SuccessShowKey qualified as SuccessShowKey
import Functional.TestArgs (TestArgs (MkTestArgs, configPath, rootDir, tmpDir))
import Functional.Timeout qualified as Timeout
import Functional.Truncation qualified as Truncation
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.IO qualified as IO
import Test.Tasty qualified as Tasty

-- | Entry point for functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler (IO.putStrLn . displayException)
  defaultMain $ Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args = do
  testGroup
    "Functional Tests"
    [ Miscellaneous.specs,
      Notify.specs,
      Success.spec args,
      SuccessCommandLogging.spec,
      SuccessFileLogging.spec args,
      SuccessShowKey.spec args,
      Timeout.spec,
      Truncation.spec,
      Readme.specs args
    ]

setup :: IO TestArgs
setup = runEff' $ do
  rootTmpDir <- (</> [osp|shrun|]) <$> PR.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> [osp|test/functional|]

  cwd <- (</> [osp|test/functional|]) <$> PR.getCurrentDirectory
  let lp = cwd </> [osp|config.toml|]

  PW.createDirectoryIfMissing True workingTmpDir
  pure
    $ MkTestArgs
      { rootDir = rootTmpDir,
        tmpDir = workingTmpDir,
        configPath = lp
      }

teardown :: TestArgs -> IO ()
teardown testArgs = do
  let root = testArgs ^. #rootDir
      cwd = testArgs ^. #tmpDir

  void $ tryAny $ runEff' $ do
    removeDirectoryIfExists cwd
    removeDirectoryIfExists $ root </> [osp|test/functional|]
    removeDirectoryIfExists $ root </> [osp|test|]
    removeDirectoryIfExists root

runEff' :: Eff [PathWriterStatic, PR.PathReaderStatic, IOE] a -> IO a
runEff' =
  runEff
    . PR.runPathReaderStaticIO
    . PW.runPathWriterStaticIO
