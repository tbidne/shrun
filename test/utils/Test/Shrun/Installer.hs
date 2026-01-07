{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitPrelude #-}

module Test.Shrun.Installer
  ( installShrunOnce,
  )
where

import Effects.FileSystem.PathReader qualified as PR
import FileSystem.OsPath (OsPath, osp, unsafeDecode, (</>))
import GHC.Stack.Types (HasCallStack)
import Test.Shrun.Process qualified as Test.Process

installShrunOnce :: (HasCallStack) => OsPath -> IO ()
installShrunOnce testDir =
  PR.findExecutable exeExpectedPath >>= \case
    Just _ -> pure ()
    Nothing -> installShrun testDir
  where
    exeExpectedPath = testDir </> [osp|shrun|]

installShrun :: (HasCallStack) => OsPath -> IO ()
installShrun testDir = Test.Process.runProcessOrDie cmd
  where
    cmd =
      mconcat
        [ "export SHRUN_HOME=$(pwd); ",
          "cabal install exe:shrun ",
          "--installdir=",
          testDirStr,
          " --install-method=copy ",
          "--overwrite-policy=always"
        ]

    testDirStr = unsafeDecode testDir
