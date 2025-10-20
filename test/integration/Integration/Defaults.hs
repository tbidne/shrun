{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Defaults (specs) where

import Integration.Prelude
import Integration.Utils
  ( CompareField (MkCompareField),
    defaultConfig,
    makeConfigAndAssertEq,
    makeConfigAndAssertFieldEq,
    notifySystemOSDBus,
    notifySystemOSNotifySend,
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
  ( ReadStrategy (ReadBlock, ReadBlockLineBuffer),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP (MkCommonLoggingP, debug, keyHide),
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
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact
      ),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (MkDeleteOnSuccessSwitch),
    FileLogInitP
      ( MkFileLogInitP,
        mode,
        path,
        sizeMode
      ),
    FileLoggingP
      ( MkFileLoggingP,
        commandNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.FileLogging.FileMode
  ( FileMode
      ( FileModeAppend,
        FileModeWrite
      ),
  )
import Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault
      ( FPDefault,
        FPManual
      ),
  )
import Shrun.Configuration.Data.FileLogging.FileSizeMode
  ( FileSizeMode (FileSizeModeWarn),
  )
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commandGraph,
        commands,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
  )
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (AppleScript, DBus, NotifySend),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
import Shrun.Configuration.Data.StripControl
  ( StripControl
      ( StripControlAll,
        StripControlNone,
        StripControlSmart
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Test.Tasty.Hedgehog (testProperty)

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile testArgs,
      cliOverridesConfigFileCmdLog,
      cliOverridesConfigFileFileLog,
      fileLogStripControlDefaultsAll,
      ignoresDefaultConfigFile,
      cliDisabledToml
    ]

defaultEnv :: TestTree
defaultEnv = testPropertyNamed desc "defaultEnv"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertEq ["cmd"] (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    ["No default config found at: './config.toml'"] === logs
  where
    desc = "No arguments and empty config path should return default Env"
    expected = defaultConfig

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testPropertyNamed desc "usesDefaultConfigFile"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertEq ["cmd1"] (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "No arguments should use config from default file"
    expected =
      MkMergedConfig
        { coreConfig =
            MkCoreConfigP
              { timeout = With 3_600,
                init = Just ". some file",
                commonLogging =
                  MkCommonLoggingP
                    { debug = MkDebug True,
                      keyHide = MkKeyHideSwitch True
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { commandLogging = MkConsoleLogCmdSwitch True,
                      commandNameTrunc = Just 80,
                      lineTrunc = Just 150,
                      stripControl = StripControlAll,
                      timerFormat = DigitalFull
                    },
                commandLogging =
                  MkCommandLoggingP
                    { bufferLength = 20,
                      bufferTimeout = fromℤ 60,
                      pollInterval = 127,
                      readSize = MkReadSize $ MkBytes 20,
                      readStrategy = ReadBlockLineBuffer,
                      reportReadErrors = MkReportReadErrorsSwitch True
                    },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { file =
                          MkFileLogInitP
                            { path = FPDefault,
                              mode = FileModeAppend,
                              sizeMode = FileSizeModeWarn $ fromℤ 50_000_000
                            },
                        commandNameTrunc = Just 45,
                        lineTrunc = Just 200,
                        deleteOnSuccess = MkDeleteOnSuccessSwitch False,
                        stripControl = StripControlNone
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
    commands = MkCommandP (mkIdx 1) (Just "cmd1") "echo \"command one\"" :<|| []

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testPropertyNamed desc "cliOverridesConfigFile"
  $ withTests 1
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|cli-log|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logPathStr = unsafeDecode logPath

    makeConfigAndAssertEq (args logPathStr) (`runConfigIO` logsRef) (expected logPath)

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI args overrides config file"
    args logPath =
      [ "--config",
        getIntConfigOS "overridden",
        "--timeout",
        "10",
        "--init",
        ". another file",
        "--file-log",
        logPath,
        "--file-log-strip-control",
        "off",
        "--file-log-command-name-trunc",
        "35",
        "--file-log-delete-on-success",
        "on",
        "--file-log-line-trunc",
        "180",
        "--console-log-command",
        "on",
        "--common-log-debug",
        "on",
        "--common-log-key-hide",
        "on",
        "--command-log-buffer-length",
        "40",
        "--command-log-buffer-timeout",
        "80",
        "--command-log-poll-interval",
        "127",
        "--command-log-read-size",
        "512 b",
        "--command-log-read-strategy",
        "block",
        "--command-log-report-read-errors",
        "on",
        "--console-log-timer-format",
        "digital_compact",
        "--console-log-command-name-trunc",
        "10",
        "--console-log-line-trunc",
        "60",
        "--console-log-strip-control",
        "off",
        "--notify-action",
        "final",
        "--notify-timeout",
        "10"
      ]
        ++ notifySendArgs
        ++ ["cmd"]
    expected logPath =
      MkMergedConfig
        { coreConfig =
            MkCoreConfigP
              { timeout = With 10,
                init = Just ". another file",
                commonLogging =
                  MkCommonLoggingP
                    { debug = MkDebug True,
                      keyHide = MkKeyHideSwitch True
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { commandLogging = MkConsoleLogCmdSwitch True,
                      commandNameTrunc = Just 10,
                      lineTrunc = Just 60,
                      stripControl = StripControlNone,
                      timerFormat = DigitalCompact
                    },
                commandLogging =
                  MkCommandLoggingP
                    { bufferLength = 40,
                      bufferTimeout = fromℤ 80,
                      pollInterval = 127,
                      readSize = MkReadSize $ MkBytes 512,
                      readStrategy = ReadBlock,
                      reportReadErrors = MkReportReadErrorsSwitch True
                    },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { file =
                          MkFileLogInitP
                            { path = FPManual logPath,
                              mode = FileModeWrite,
                              sizeMode = FileSizeModeWarn $ fromℤ 50_000_000
                            },
                        commandNameTrunc = Just 35,
                        deleteOnSuccess = MkDeleteOnSuccessSwitch True,
                        lineTrunc = Just 180,
                        stripControl = StripControlNone
                      },
                notify =
                  Just
                    $ MkNotifyP
                      { action = NotifyFinal,
                        system = notifySystemOSNotifySend,
                        timeout = NotifyTimeoutSeconds 10
                      }
              },
          commandGraph = Graph.mkTrivialGraph commands,
          commands
        }
    commands = MkCommandP (mkIdx 1) Nothing "cmd" :<|| []

cliOverridesConfigFileCmdLog :: TestTree
cliOverridesConfigFileCmdLog = testPropertyNamed desc "cliOverridesConfigFileCmdLog"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI overrides config file command-log fields even when CLI --console-log-command is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--console-log-line-trunc",
        "60",
        "--console-log-strip-control",
        "off",
        "cmd"
      ]
    expected =
      [ #coreConfig % #consoleLogging % #stripControl ^=@ StripControlNone,
        #coreConfig % #consoleLogging % #lineTrunc % _Just ^?=@ Just 60
      ]

cliOverridesConfigFileFileLog :: TestTree
cliOverridesConfigFileFileLog = testPropertyNamed desc "cliOverridesConfigFileFileLog"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI overrides config file file-log fields even when CLI --file-log is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--file-log-command-name-trunc",
        "55",
        "--file-log-delete-on-success",
        "on",
        "--file-log-line-trunc",
        "180",
        "--file-log-mode",
        "write",
        "--file-log-strip-control",
        "smart",
        "--file-log-size-mode",
        "warn 10 mb",
        "cmd"
      ]

    expected =
      [ #coreConfig % #fileLogging %? #commandNameTrunc ^?=@ Just (Just 55),
        #coreConfig % #fileLogging %? #deleteOnSuccess % #unDeleteOnSuccessSwitch ^?=@ Just True,
        #coreConfig % #fileLogging %? #lineTrunc ^?=@ Just (Just 180),
        #coreConfig % #fileLogging %? #stripControl ^?=@ Just StripControlSmart,
        #coreConfig % #fileLogging %? #file % #mode ^?=@ Just FileModeWrite,
        #coreConfig % #fileLogging %? #file % #sizeMode ^?=@ Just (FileSizeModeWarn $ MkBytes 10_000_000)
      ]

fileLogStripControlDefaultsAll :: TestTree
fileLogStripControlDefaultsAll = testPropertyNamed desc "fileLogStripControlDefaultsAll"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    -- Test that no toml defaults to All
    makeConfigAndAssertFieldEq args1 (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    ["No default config found at: './config.toml'"] === logs

    -- Test that with toml defaults to All
    makeConfigAndAssertFieldEq args2 (`runNoConfigIO` logsRef) expected
  where
    desc = "File log strip-control defaults to All"
    args1 =
      [ "--file-log",
        "default",
        "cmd"
      ]

    args2 =
      [ "--config",
        getIntConfig "basic-file-log",
        "--file-log",
        "default",
        "cmd"
      ]

    expected =
      [ #coreConfig % #fileLogging %? #stripControl ^?=@ Just StripControlAll
      ]

ignoresDefaultConfigFile :: TestTree
ignoresDefaultConfigFile = testPropertyNamed desc "ignoresDefaultConfigFile"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertEq ["--config", "off", "cmd"] (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--config off should ignore config file"
    expected = defaultConfig

cliDisabledToml :: TestTree
cliDisabledToml = testPropertyNamed desc "cliDisabledToml"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "cli disables toml options"
    args =
      [ "--command-log-report-read-errors",
        "off",
        "--common-log-key-hide",
        "off",
        "--config",
        getIntConfigOS "overridden",
        "--console-log-command",
        "off",
        "--console-log-command-name-trunc",
        "off",
        "--console-log-line-trunc",
        "off",
        "--init",
        "off",
        "--file-log",
        "off",
        "--edges",
        "off",
        "--notify-action",
        "off",
        "cmd"
      ]
    expected = defaultConfig

notifySendArgs :: List String
#if OSX
notifySendArgs = []
#else
notifySendArgs = [ "--notify-system", "notify-send" ]
#endif
