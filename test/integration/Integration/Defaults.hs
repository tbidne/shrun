{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Defaults (specs) where

import Effects.FileSystem.Utils qualified as FsUtils
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
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingP (MkCmdLoggingP, pollInterval, readSize),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP (MkCommonLoggingP, keyHide, timerFormat),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (ConsoleLogCmdOn),
    ConsoleLoggingP
      ( MkConsoleLoggingP,
        cmdLogging,
        cmdNameTrunc,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        cmdLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (DeleteOnSuccessOff, DeleteOnSuccessOn),
    FileLogInitP
      ( MkFileLogInitP,
        mode,
        path,
        sizeMode
      ),
    FileLoggingP
      ( MkFileLoggingP,
        cmdNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commands,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Data.CmdLogReadSize (CmdLogReadSize (MkCmdLogReadSize))
import Shrun.Data.Command (CommandP (MkCommandP))
import Shrun.Data.FileMode (FileMode (FileModeAppend, FileModeWrite))
import Shrun.Data.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Data.FileSizeMode (FileSizeMode (FileSizeModeWarn))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff, KeyHideOn))
import Shrun.Data.StripControl
  ( StripControl
      ( StripControlAll,
        StripControlNone,
        StripControlSmart
      ),
  )
import Shrun.Data.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact
      ),
  )
import Shrun.Notify.Types
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
    NotifySystemP (AppleScript, DBus, NotifySend),
    NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
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
      noXOverridesToml,
      noXOverridesArgs
    ]

defaultEnv :: TestTree
defaultEnv = testPropertyNamed desc "defaultEnv"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertEq ["cmd"] (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    ["No default config found at: ./config.toml"] === logs
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
              { timeout = Just 3_600,
                init = Just ". some file",
                commonLogging =
                  MkCommonLoggingP
                    { keyHide = KeyHideOn,
                      timerFormat = DigitalFull
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { cmdLogging = ConsoleLogCmdOn,
                      cmdNameTrunc = Just 80,
                      lineTrunc = Just 150,
                      stripControl = StripControlAll
                    },
                cmdLogging =
                  MkCmdLoggingP
                    { pollInterval = 127,
                      readSize = MkCmdLogReadSize $ MkBytes 20
                    },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { file =
                          MkFileLogInitP
                            { path = FPDefault,
                              mode = FileModeAppend,
                              sizeMode = FileSizeModeWarn $ afromInteger 50_000_000
                            },
                        cmdNameTrunc = Just 45,
                        lineTrunc = Just 200,
                        deleteOnSuccess = DeleteOnSuccessOff,
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
          commands = MkCommandP (Just "cmd1") "echo \"command one\"" :<|| []
        }

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testPropertyNamed desc "cliOverridesConfigFile"
  $ withTests 1
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|cli-log|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logPathStr = FsUtils.unsafeDecodeOsToFp logPath

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
        "none",
        "--file-log-cmd-name-trunc",
        "35",
        "--file-log-delete-on-success",
        "--file-log-line-trunc",
        "180",
        "--console-log-cmd",
        "--log-key-hide",
        "--cmd-log-poll-interval",
        "127",
        "--cmd-log-read-size",
        "512",
        "--log-timer-format",
        "digital_compact",
        "--console-log-cmd-name-trunc",
        "10",
        "--console-log-line-trunc",
        "60",
        "--console-log-strip-control",
        "none",
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
              { timeout = Just 10,
                init = Just ". another file",
                commonLogging =
                  MkCommonLoggingP
                    { keyHide = KeyHideOn,
                      timerFormat = DigitalCompact
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { cmdLogging = ConsoleLogCmdOn,
                      cmdNameTrunc = Just 10,
                      lineTrunc = Just 60,
                      stripControl = StripControlNone
                    },
                cmdLogging =
                  MkCmdLoggingP
                    { pollInterval = 127,
                      readSize = MkCmdLogReadSize $ MkBytes 512
                    },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { file =
                          MkFileLogInitP
                            { path = FPManual logPath,
                              mode = FileModeAppend,
                              sizeMode = FileSizeModeWarn $ afromInteger 50_000_000
                            },
                        cmdNameTrunc = Just 35,
                        deleteOnSuccess = DeleteOnSuccessOn,
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
          commands = "cmd" :<|| []
        }

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
    desc = "CLI overrides config file cmd-log fields even when CLI --console-log-cmd is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--console-log-line-trunc",
        "60",
        "--console-log-strip-control",
        "none",
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
        "--file-log-cmd-name-trunc",
        "55",
        "--file-log-delete-on-success",
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
      [ #coreConfig % #fileLogging %? #cmdNameTrunc ^?=@ Just (Just 55),
        #coreConfig % #fileLogging %? #deleteOnSuccess ^?=@ Just DeleteOnSuccessOn,
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
    ["No default config found at: ./config.toml"] === logs

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
    makeConfigAndAssertEq ["--no-config", "cmd"] (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-config should ignore config file"
    expected = defaultConfig

noXOverridesToml :: TestTree
noXOverridesToml = testPropertyNamed desc "noXOverridesToml"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-x disables toml options"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--no-timeout",
        "--no-init",
        "--no-log-key-hide",
        "--no-cmd-log-poll-interval",
        "--no-cmd-log-read-size",
        "--no-log-timer-format",
        "--no-console-log-cmd-name-trunc",
        "--no-console-log-cmd",
        "--no-console-log-strip-control",
        "--no-console-log-line-trunc",
        "--no-file-log",
        "--no-file-log-strip-control",
        "--no-notify-action",
        "--no-notify-system",
        "--no-notify-timeout",
        "cmd"
      ]
    expected = defaultConfig

noXOverridesArgs :: TestTree
noXOverridesArgs = testPropertyNamed desc "noXOverridesArgs"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-x disables args"
    args =
      [ "--timeout",
        "5",
        "--no-timeout",
        "--init",
        "blah",
        "--no-init",
        "--cmd-log-poll-interval",
        "555",
        "--no-cmd-log-poll-interval",
        "--cmd-log-read-size",
        "512",
        "--no-cmd-log-read-size",
        "--log-timer-format",
        "prose_full",
        "--no-log-timer-format",
        "--log-key-hide",
        "--no-log-key-hide",
        "--console-log-cmd-name-trunc",
        "80",
        "--no-console-log-cmd-name-trunc",
        "--console-log-cmd",
        "--no-console-log-cmd",
        "--console-log-strip-control",
        "all",
        "--no-console-log-strip-control",
        "--console-log-line-trunc",
        "100",
        "--no-console-log-line-trunc",
        "--file-log",
        "path",
        "--no-file-log",
        "--file-log-strip-control",
        "all",
        "--no-file-log-strip-control",
        "--notify-action",
        "command",
        "--no-notify-action",
        "--notify-system",
        "dbus",
        "--no-notify-system",
        "--notify-timeout",
        "never",
        "--no-notify-timeout",
        "cmd"
      ]
    expected = defaultConfig

notifySendArgs :: List String
#if OSX
notifySendArgs = []
#else
notifySendArgs = [ "--notify-system", "notify-send" ]
#endif
