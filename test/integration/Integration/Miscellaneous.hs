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
import Shrun.Configuration.Data.MergedConfig qualified as Merged
import Shrun.Configuration.Toml qualified as Toml
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.FileSizeMode (FileSizeMode (FileSizeModeNothing))
import Shrun.Env (withEnv)

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Miscellaneous"
    [ logFileWarn testArgs,
      logFileDelete testArgs,
      usesRecursiveCmdExample,
      usesRecursiveCmd,
      lineTruncDetect,
      testFileSizeModeNothing,
      testDefaultConfigs
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testPropertyNamed desc "logFileWarn" $ property $ do
  logPath <- liftIO $ (</> [osp|large-file-warn|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logsPathStr = FsUtils.unsafeDecodeOsToFp logPath
      contents = T.replicate 1_500 "test "

      run = liftIO $ do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logsPathStr) (withEnv pure)

  run

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
logFileDelete testArgs = testPropertyNamed desc "logFileDelete" $ property $ do
  logPath <- liftIO $ (</> [osp|large-file-del|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logPathStr = FsUtils.unsafeDecodeOsToFp logPath
      contents = T.replicate 1_500 "test "

      run = liftIO $ do
        writeFileUtf8 logPath contents

        flip runConfigIO logsRef $ withArgs (args logPathStr) (withEnv pure)

        -- file should have been deleted then recreated with a file size of 0.
        getFileSize logPath

  size <- run
  0 === size

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

usesRecursiveCmdExample :: TestTree
usesRecursiveCmdExample = testPropertyNamed desc "usesRecursiveCmdExample"
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
      MkCommand (Just "m1") "m1val"
        :<|| [ "m2",
               "m3"
             ]
    expected = [#commands ^=@ cmds]

usesRecursiveCmd :: TestTree
usesRecursiveCmd = testPropertyNamed desc "usesRecursiveCmd" $ property $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  [] === logs
  where
    desc = "Uses recursive commands"
    args = ["-c", getExampleConfig "default", "all", "echo cat"]

    cmds =
      MkCommand (Just "cmd1") "echo \"command one\""
        :<|| [ MkCommand (Just "cmd4") "command four",
               "echo hi",
               "echo cat"
             ]
    expected = [#commands ^=@ cmds]

lineTruncDetect :: TestTree
lineTruncDetect = testPropertyNamed desc "lineTruncDetect" $ property $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "cmdLogLineTrunc reads 'detect' string from toml"
    args = ["-c", getIntConfig "misc", "cmd1"]

    expected = [#coreConfig % #cmdLogging %? #lineTrunc % _Just ^?=@ Just 87]

testFileSizeModeNothing :: TestTree
testFileSizeModeNothing = testPropertyNamed desc "testFileSizeModeNothing" $ property $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "size-mode reads 'nothing'"
    args = ["-c", getIntConfig "basic-file-log", "cmd"]

    expected = [#coreConfig % #fileLogging %? #sizeMode ^?=@ Just FileSizeModeNothing]

newtype TermIO a = MkTermIO (IO a)
  deriving (Applicative, Functor, Monad, MonadThrow) via IO

runTermIO :: (MonadIO m) => TermIO a -> m a
runTermIO (MkTermIO a) = liftIO a

-- For MonadTerminal instance (used to get window size; shouldn't be used
-- in default configs)
instance MonadTerminal TermIO

testDefaultConfigs :: TestTree
testDefaultConfigs = testPropertyNamed desc "testDefaultConfigs" $ property $ do
  let expected = Merged.defaultMergedConfig cmds
      args = Args.defaultArgs cmds
      toml = Toml.defaultToml

  resultNoToml <- runTermIO $ Configuration.mergeConfig args Nothing
  resultMerge <- runTermIO $ Configuration.mergeConfig args $ Just toml

  expected === resultNoToml
  expected === resultMerge
  where
    desc = "defaultMergedConfig === merge defaultArgs defaultToml"

    cmds :: (IsString a) => NESeq a
    cmds = NESeq.singleton "cmd"
