{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Miscellaneous (specs) where

import Data.Sequence.NonEmpty qualified as NESeq
import Data.String (IsString)
import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Integration.Prelude
import Integration.Utils
  ( makeConfigAndAssertFieldEq,
    runConfigIO,
    runNoConfigIO,
    (^=@),
    (^?=@),
  )
import Shrun.Configuration qualified as Configuration
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (DeleteOnSuccessOn),
  )
import Shrun.Configuration.Data.FileLogging.FileSizeMode
  ( FileSizeMode (FileSizeModeNothing),
  )
import Shrun.Configuration.Data.MergedConfig qualified as Merged
import Shrun.Configuration.Default (Default (def))
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Toml (Toml)
import Shrun.Data.Command (CommandP (MkCommandP))

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    [ logFileWarn testArgs,
      logFileDelete testArgs,
      logFileNothing testArgs,
      usesRecursiveCmdExample,
      usesRecursiveCmd,
      lineTruncDetect,
      testFileLogDeleteOnSuccess,
      testFileSizeModeNothing,
      testDefaultConfigs
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testPropertyNamed desc "logFileWarn"
  $ withTests 1
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|large-file-warn|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logsPathStr = FsUtils.unsafeDecodeOsToFp logPath
        contents = T.replicate 1_500 "test "

        run = liftIO $ do
          writeFileUtf8 logPath contents
          startSize <- getFileSize logPath

          flip runConfigIO logsRef $ withArgs (args logsPathStr) (withEnv pure)

          endSize <- getFileSize logPath
          pure (startSize, endSize)

    (startSize, endSize) <- run

    exists <- liftIO $ doesFileExist logPath
    assert exists

    -- NOTE: [Log file unchanged]
    --
    -- In a real run this would be >=, but we are not actually running
    -- shrun so the file should stay untouched.
    endSize === startSize

    logs <- liftIO $ readIORef logsRef
    [warning logsPathStr] === logs
  where
    desc = "Large log file should print warning"
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "warn 5.5 kb",
        "cmd"
      ]

logFileDelete :: IO TestArgs -> TestTree
logFileDelete testArgs = testPropertyNamed desc "logFileDelete"
  $ withTests 1
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|large-file-del|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logPathStr = FsUtils.unsafeDecodeOsToFp logPath
        contents = T.replicate 1_500 "test "

        run = liftIO $ do
          writeFileUtf8 logPath contents

          flip runConfigIO logsRef $ withArgs (args logPathStr) (withEnv pure)

          getFileSize logPath

    endSize <- run

    exists <- liftIO $ doesFileExist logPath
    assert exists

    -- file should have been deleted then recreated with a file size of 0.
    0 === endSize

    logs <- liftIO $ readIORef logsRef
    [warning logPathStr] === logs
  where
    desc = "Large log file should be deleted"
    warning fp =
      mconcat
        [ "Warning: log file '",
          T.pack fp,
          "' has size: 7.50 kb, ",
          "but specified threshold is: 5.50 kb. Deleting log."
        ]
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "delete 5.5 kilobytes",
        "cmd"
      ]

logFileNothing :: IO TestArgs -> TestTree
logFileNothing testArgs = testPropertyNamed desc "logFileNothing"
  $ withTests 1
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|large-file-nothing|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logsPathStr = FsUtils.unsafeDecodeOsToFp logPath
        contents = T.replicate 1_500 "test "

        run = liftIO $ do
          writeFileUtf8 logPath contents
          startSize <- getFileSize logPath

          flip runConfigIO logsRef $ withArgs (args logsPathStr) (withEnv pure)

          endSize <- getFileSize logPath
          pure (startSize, endSize)

    (startSize, endSize) <- run

    exists <- liftIO $ doesFileExist logPath
    assert exists

    -- see NOTE: [Log file unchanged]
    endSize === startSize

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "Large log file should print warning"
    args fp =
      [ "-f",
        fp,
        "--file-log-size-mode",
        "nothing",
        "cmd"
      ]

usesRecursiveCmdExample :: TestTree
usesRecursiveCmdExample = testPropertyNamed desc "usesRecursiveCmdExample"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "Uses recursive command from example"
    args = ["multi1"]
    cmds =
      MkCommandP (Just "m1") "m1val"
        :<|| [ "m2",
               "m3"
             ]
    expected = [#commands ^=@ cmds]

usesRecursiveCmd :: TestTree
usesRecursiveCmd = testPropertyNamed desc "usesRecursiveCmd"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "Uses recursive commands"
    args = ["-c", getExampleConfigOS "config", "all", "echo cat"]

    cmds =
      MkCommandP (Just "cmd1") "echo \"command one\""
        :<|| [ MkCommandP (Just "cmd4") "command four",
               "echo hi",
               "echo cat"
             ]
    expected = [#commands ^=@ cmds]

lineTruncDetect :: TestTree
lineTruncDetect = testPropertyNamed desc "lineTruncDetect"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    logs === []
  where
    desc = "lineTrunc reads 'detect' string from toml"
    args = ["-c", getIntConfig "misc", "cmd1"]

    expected =
      [ #coreConfig % #consoleLogging % #lineTrunc % _Just ^?=@ Just 86,
        #coreConfig % #fileLogging %? #lineTrunc % _Just ^?=@ Just 86
      ]

testFileSizeModeNothing :: TestTree
testFileSizeModeNothing = testPropertyNamed desc "testFileSizeModeNothing"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    logs === []
  where
    desc = "size-mode reads 'nothing'"
    args = ["-c", getIntConfig "basic-file-log", "cmd"]

    expected = [#coreConfig % #fileLogging %? #file % #sizeMode ^?=@ Just FileSizeModeNothing]

testFileLogDeleteOnSuccess :: TestTree
testFileLogDeleteOnSuccess = testPropertyNamed desc "testFileLogDeleteOnSuccess"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    logs === []
  where
    desc = "delete-on-success reads true"
    args = ["-c", getIntConfig "basic-file-log", "cmd"]

    expected = [#coreConfig % #fileLogging %? #deleteOnSuccess ^?=@ Just DeleteOnSuccessOn]

newtype TermIO a = MkTermIO (IO a)
  deriving (Applicative, Functor, Monad, MonadThrow) via IO

runTermIO :: (MonadIO m) => TermIO a -> m a
runTermIO (MkTermIO a) = liftIO a

-- For MonadTerminal instance (used to get window size; shouldn't be used
-- in default configs)
instance MonadTerminal TermIO

testDefaultConfigs :: TestTree
testDefaultConfigs = testPropertyNamed desc "testDefaultConfigs"
  $ withTests 1
  $ property
  $ do
    let expected = Merged.defaultMergedConfig cmds
        args = Args.defaultArgs cmds
        toml = def @Toml

    resultNoToml <- runTermIO $ Configuration.mergeConfig args Nothing
    resultMerge <- runTermIO $ Configuration.mergeConfig args $ Just toml

    expected === resultNoToml
    expected === resultMerge
  where
    desc = "defaultMergedConfig === merge defaultArgs defaultToml"

    cmds :: (IsString a) => NESeq a
    cmds = NESeq.singleton "cmd"
