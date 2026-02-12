{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Shrun.Prelude hiding (IO)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Shrun.Installer qualified as Test.Installer
import Test.Shrun.Logger qualified as Test.Logger
import Exe.Help qualified as Help
import Exe.Terminate qualified as Terminate
import Prelude (IO)

-- | Entry point for executable tests.
main :: IO ()
main = do
  guardOrElse' "TEST_EXE" ExpectEnvSet runTests dontRun
  where
    runTests = bracket setup teardown tests
    dontRun = Test.Logger.putLog "Executable tests disabled. Enable with TEST_EXE=1"

    tests testDir =
      Help.tests testDir
        *> Terminate.tests testDir

setup :: (HasCallStack) => IO OsPath
setup = do
  tmpDir <- PR.getTemporaryDirectory
  let testDir = tmpDir </> [ospPathSep|shrun/test/exe|]

  PW.createDirectoryIfMissing True testDir

  Test.Installer.installShrunOnce testDir

  pure testDir

teardown :: (HasCallStack) => OsPath -> IO ()
teardown = PW.removePathForciblyIfExists_
