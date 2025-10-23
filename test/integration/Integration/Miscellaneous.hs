{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Miscellaneous (specs) where

import Data.Text qualified as T
import Integration.Prelude
import Integration.Utils
  ( makeConfigAndAssertEq,
    makeConfigAndAssertFieldEq,
    notifySystemOSDBus,
    runConfigIO,
    runNoConfigIO,
    (^=@),
    (^?=@),
  )
import Shrun.Command.Types (CommandP (MkCommandP))
import Shrun.Configuration.Data.CommandLogging
  ( CommandLoggingP
      ( MkCommandLoggingP,
        bufferLength,
        bufferTimeout,
        pollInterval,
        readSize,
        readStrategy,
        reportReadErrors
      ),
    ReportReadErrorsSwitch (MkReportReadErrorsSwitch),
  )
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize (MkReadSize))
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock,
        ReadBlockLineBuffer
      ),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP
      ( MkCommonLoggingP,
        debug,
        keyHide
      ),
    Debug (MkDebug),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (MkKeyHideSwitch),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (MkConsoleLogCmdSwitch),
    ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat (DigitalCompact, ProseCompact))
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        legendKeysCache,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (MkDeleteOnSuccessSwitch),
    FileLogInitP (MkFileLogInitP, mode, path, sizeMode),
    FileLoggingP
      ( MkFileLoggingP,
        commandNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.FileLogging.FileMode (FileMode (FileModeRename, FileModeWrite))
import Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault (FPManual),
    _FPManual,
  )
import Shrun.Configuration.Data.FileLogging.FileSizeMode (FileSizeMode (FileSizeModeNothing, FileSizeModeWarn))
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Data.LegendKeysCache
  ( LegendKeysCache
      ( LegendKeysAdd,
        LegendKeysOff
      ),
  )
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (MkMergedConfig, commandGraph, commands, coreConfig),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Action (NotifyAction (NotifyAll))
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever),
  )
import Shrun.Configuration.Data.StripControl (StripControl (StripControlAll, StripControlSmart))
import Shrun.Configuration.Data.Truncation
  ( Truncation (MkTruncation, unTruncation),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Configuration.Env (withEnv)

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
      testReadBlockLineBufferReadStrategy,
      testConfigsMerged,
      testConfigsMergedDisabled,
      testOverridesDuplicate
    ]

logFileWarn :: IO TestArgs -> TestTree
logFileWarn testArgs = testProp1 desc "logFileWarn" $ do
  logPath <- liftIO $ (</> [osp|large-file-warn|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logsPathStr = unsafeDecode logPath
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
logFileDelete testArgs = testProp1 desc "logFileDelete" $ do
  logPath <- liftIO $ (</> [osp|large-file-del|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logPathStr = unsafeDecode logPath
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
logFileNothing testArgs = testProp1 desc "logFileNothing" $ do
  logPath <- liftIO $ (</> [osp|large-file-nothing|]) . view #workingTmpDir <$> testArgs
  logsRef <- liftIO $ newIORef []
  let logsPathStr = unsafeDecode logPath
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
        "off",
        "cmd"
      ]

usesRecursiveCmdExample :: TestTree
usesRecursiveCmdExample = testProp1 desc "usesRecursiveCmdExample" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  [] === logs
  where
    desc = "Uses recursive command from example"
    args = ["multi1"]
    cmds =
      MkCommandP (mkIdx 1) (Just "m1") "m1val"
        :<|| [ MkCommandP (mkIdx 2) Nothing "m2",
               MkCommandP (mkIdx 3) Nothing "m3"
             ]
    expected = [#commands ^=@ cmds]

usesRecursiveCmd :: TestTree
usesRecursiveCmd = testProp1 desc "usesRecursiveCmd" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  [] === logs
  where
    desc = "Uses recursive commands"
    args = ["-c", getExampleConfigOS, "all", "echo cat"]

    cmds =
      MkCommandP (mkIdx 1) (Just "cmd1") "echo \"command one\""
        :<|| [ MkCommandP (mkIdx 2) (Just "cmd4") "command four",
               MkCommandP (mkIdx 3) Nothing "echo hi",
               MkCommandP (mkIdx 4) Nothing "echo cat"
             ]
    expected = [#commands ^=@ cmds]

lineTruncDetect :: TestTree
lineTruncDetect = testProp1 desc "lineTruncDetect" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "lineTrunc reads 'detect' string from toml"
    args = ["-c", getIntConfig "misc", "cmd1"]

    expected =
      [ #coreConfig % #consoleLogging % #lineTrunc % _Just ^?=@ Just 86,
        #coreConfig % #fileLogging %? #lineTrunc % _Just ^?=@ Just 86,
        #coreConfig % #fileLogging %? #file % #mode ^?=@ Just FileModeRename
      ]

testFileSizeModeNothing :: TestTree
testFileSizeModeNothing = testProp1 desc "testFileSizeModeNothing" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "size-mode reads 'nothing'"
    args = ["-c", "off", "-c", getIntConfig "basic-file-log", "cmd"]

    expected = [#coreConfig % #fileLogging %? #file % #sizeMode ^?=@ Just FileSizeModeNothing]

testFileLogDeleteOnSuccess :: TestTree
testFileLogDeleteOnSuccess = testProp1 desc "testFileLogDeleteOnSuccess" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "delete-on-success reads true"
    args = ["-c", "off", "-c", getIntConfig "basic-file-log", "cmd"]

    expected = [#coreConfig % #fileLogging %? #deleteOnSuccess % #unDeleteOnSuccessSwitch ^?=@ Just True]

testReadBlockLineBufferReadStrategy :: TestTree
testReadBlockLineBufferReadStrategy = testProp1 desc "testReadBlockLineBufferReadStrategy" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runNoConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "Read block-line-buffer read-strategy"
    args = ["-c", "off", "-c", getIntConfig "config", "cmd"]

    expected = [#coreConfig % #commandLogging % #readStrategy ^=@ ReadBlockLineBuffer]

testConfigsMerged :: TestTree
testConfigsMerged = testProp1 desc "testConfigsMerged" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertEq args (`runConfigIO` logsRef) expectedMultiConfig

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "Multiple toml files are merged"

    args =
      [ "-c",
        getIntConfig "cfg1",
        "-c",
        getIntConfig "cfg2",
        "-c",
        getIntConfig "cfg3"
      ]
        ++ multiTomlCommands

testConfigsMergedDisabled :: TestTree
testConfigsMergedDisabled = testProp1 desc "testConfigsMergedDisabled" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "Multiple toml files are merged with disabling"

    args =
      [ "-c",
        getIntConfig "cfg1",
        "-c",
        "off",
        "-c",
        getIntConfig "cfg2",
        "-c",
        getIntConfig "cfg3"
      ]
        ++ multiTomlCommands

    -- We disable xdg and cfg1. Hence this config should be similar to
    -- expectedMultiConfig, but with some fields changed.
    expected =
      setMany' @List
        [ -- commands
          MkSomeSetter #commands commands,
          MkSomeSetter #commandGraph (Graph.mkTrivialGraph commands),
          -- core
          MkSomeSetter (#coreConfig % #legendKeysCache) LegendKeysAdd,
          MkSomeSetter (#coreConfig % #timeout) Disabled,
          -- command logging
          MkSomeSetter (#coreConfig % #commandLogging % #bufferLength) 1000,
          MkSomeSetter (#coreConfig % #commandLogging % #bufferTimeout % #unBufferTimeout) 30,
          MkSomeSetter (#coreConfig % #commandLogging % #readSize % #unReadSize % #unBytes) 16_000,
          MkSomeSetter (#coreConfig % #commandLogging % #reportReadErrors % #unReportReadErrorsSwitch) False,
          -- common logging
          MkSomeSetter (#coreConfig % #commonLogging % #debug % #unDebug) False,
          MkSomeSetter (#coreConfig % #commonLogging % #keyHide % #unKeyHideSwitch) False,
          -- console logging
          MkSomeSetter (#coreConfig % #consoleLogging % #lineTrunc) Nothing,
          MkSomeSetter (#coreConfig % #consoleLogging % #stripControl) StripControlSmart,
          MkSomeSetter (#coreConfig % #consoleLogging % #timerFormat) ProseCompact,
          -- file logging
          MkSomeSetter (#coreConfig % #fileLogging %? #file % #path % _FPManual) [osp|cfg3 file|],
          -- notify
          MkSomeSetter (#coreConfig % #notify) Nothing
        ]
        expectedMultiConfig

    commands =
      unsafeListToNESeq
        [ MkCommandP (mkIdx 1) Nothing "cmd1",
          MkCommandP (mkIdx 2) Nothing "cfg1_1",
          MkCommandP (mkIdx 3) (Just "cfg2_1") "cfg2 val 1",
          MkCommandP (mkIdx 4) (Just "cfg3_1") "cfg3 val 1",
          MkCommandP (mkIdx 5) (Just "cfg3_2") "cfg3 val 2"
        ]

testOverridesDuplicate :: TestTree
testOverridesDuplicate = testProp1 desc "testOverridesDuplicate" $ do
  logsRef <- liftIO $ newIORef []
  makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef logsRef
  logs === []
  where
    desc = "Config overrides duplicates without error"

    args =
      [ "-c",
        getIntConfig "duplicate-keys",
        "-c",
        getIntConfig "duplicate-keys-override",
        "key1"
      ]

    expected =
      [ #commands ^=@ unsafeListToNESeq [MkCommandP (mkIdx 1) (Just "key1") "val override"]
      ]

expectedMultiConfig :: MergedConfig
expectedMultiConfig =
  MkMergedConfig
    { coreConfig =
        MkCoreConfigP
          { timeout = With 87,
            init = Just "cfg3 init",
            legendKeysCache = LegendKeysOff,
            commonLogging =
              MkCommonLoggingP
                { debug = MkDebug True,
                  keyHide = MkKeyHideSwitch True
                },
            consoleLogging =
              MkConsoleLoggingP
                { commandLogging = MkConsoleLogCmdSwitch False,
                  commandNameTrunc = Just 200,
                  lineTrunc = Just 150,
                  stripControl = StripControlAll,
                  timerFormat = DigitalCompact
                },
            commandLogging =
              MkCommandLoggingP
                { bufferLength = 20,
                  bufferTimeout = fromℤ 60,
                  pollInterval = 130,
                  readSize = MkReadSize $ MkBytes 20,
                  readStrategy = ReadBlock,
                  reportReadErrors = MkReportReadErrorsSwitch True
                },
            fileLogging =
              Just
                $ MkFileLoggingP
                  { file =
                      MkFileLogInitP
                        { path = FPManual [osp|cfg3 file|],
                          mode = FileModeWrite,
                          sizeMode = FileSizeModeWarn (MkBytes 50_000_000)
                        },
                    commandNameTrunc = Just $ MkTruncation {unTruncation = 123},
                    deleteOnSuccess = MkDeleteOnSuccessSwitch False,
                    lineTrunc = Just $ MkTruncation {unTruncation = 300},
                    stripControl = StripControlSmart
                  },
            notify =
              Just
                $ MkNotifyP
                  { action = NotifyAll,
                    system = notifySystemOSDBus,
                    timeout = NotifyTimeoutNever
                  }
          },
      commandGraph = Graph.mkTrivialGraph commands,
      commands
    }
  where
    commands =
      unsafeListToNESeq
        [ MkCommandP (mkIdx 1) (Just "cmd1") "echo \"command one\"",
          MkCommandP (mkIdx 2) (Just "cfg1_1") "cfg1 val 1",
          MkCommandP (mkIdx 3) (Just "cfg2_1") "cfg2 val 1",
          MkCommandP (mkIdx 4) (Just "cfg3_1") "cfg3 val 1",
          MkCommandP (mkIdx 5) (Just "cfg3_2") "cfg3 val 2"
        ]

multiTomlCommands :: List String
multiTomlCommands =
  [ -- xdg
    "cmd1",
    -- cfg1, cfg2, cfg3
    "cfg1_1",
    "cfg2_1",
    "cfg3_1",
    "cfg3_2"
  ]

data SomeSetter p where
  MkSomeSetter ::
    forall p x k ix.
    (Is k A_Setter) =>
    Optic k ix p p x x ->
    x ->
    SomeSetter p

-- | Targets some object using a list of optics and the new values.
-- Equivalent to calling set' multiple times.
setMany' ::
  forall f p.
  (Foldable f) =>
  f (SomeSetter p) ->
  p ->
  p
setMany' ls s = foldl' (\s' (MkSomeSetter l x) -> set' l x s') s ls
