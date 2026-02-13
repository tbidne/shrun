{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Exe.Help qualified as Help
import Exe.Terminate qualified as Terminate
import FileSystem.OsPath (unsafeDecode)
import Shrun.Prelude hiding (IO)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Shrun.Installer qualified as Test.Installer
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO)

-- | Entry point for executable tests.
main :: IO ()
main = do
  guardOrElse' "TEST_EXE" ExpectEnvSet runTests dontRun
  where
    runTests = bracket setup teardown $ \testDir ->
      defaultMain
        $ testGroup
          "Unit tests"
          [ Help.tests testDir,
            Terminate.tests testDir
          ]
    dontRun = putStrLn "*** Executable tests disabled. Enable with TEST_EXE=1 ***"

setup :: (HasCallStack) => IO OsPath
setup = do
  tmpDir <- PR.getTemporaryDirectory
  let testDir = tmpDir </> [ospPathSep|shrun/test/exe|]
      testDirStr = unsafeDecode testDir

  dirExists <- PR.doesDirectoryExist testDir
  if dirExists
    then do
      putStrLn
        $ mconcat
          [ "*** Test dir '",
            testDirStr,
            "' already exists, deleting non-exe contents. ***"
          ]
      -- If testDir exists, delete all contents (e.g. log files) _except_
      -- executable.
      contents <- PR.listDirectory testDir
      for_ contents $ \p -> do
        unless (p == [osp|shrun|]) $ do
          PW.removePathForcibly p
    else
      PW.createDirectoryIfMissing True testDir

  -- If exe does not exist, install it.
  exeExists <- PR.doesPathExist (testDir </> [osp|shrun|])
  if exeExists
    then putStrLn "*** shrun exe exists, skipping installation. ***"
    else Test.Installer.installShrunOnce testDir

  pure testDir

teardown :: (HasCallStack) => OsPath -> IO ()
teardown testDir =
  guardOrElse'
    "NO_CLEANUP"
    ExpectEnvSet
    doNothing
    (PW.removePathForciblyIfExists_ testDir)
  where
    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient testDir
        <> "'"
