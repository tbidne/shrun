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
  ( CmdLoggingP (MkCmdLoggingP, lineTrunc, stripControl),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        cmdLogSize,
        cmdLogging,
        cmdNameTrunc,
        fileLogging,
        init,
        keyHide,
        notify,
        pollInterval,
        timeout,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( FileLoggingP
      ( MkFileLoggingP,
        mode,
        path,
        sizeMode,
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
import Shrun.Data.Command (Command (MkCommand))
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
    NotifySystem (AppleScript, DBus, NotifySend),
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
                keyHide = KeyHideOn,
                pollInterval = 127,
                cmdLogSize = MkBytes 20,
                timerFormat = DigitalFull,
                cmdNameTrunc = Just 80,
                cmdLogging =
                  Just
                    $ MkCmdLoggingP
                      { stripControl = StripControlAll,
                        lineTrunc = Just 150
                      },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { path = FPDefault,
                        stripControl = StripControlNone,
                        mode = FileModeAppend,
                        sizeMode = Nothing
                      },
                notify =
                  Just
                    $ MkNotifyP
                      { action = NotifyAll,
                        system = notifySystemOSDBus,
                        timeout = NotifyTimeoutNever
                      }
              },
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
        }

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testPropertyNamed desc "cliOverridesConfigFile"
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
        "--cmd-log",
        "--key-hide",
        "--poll-interval",
        "127",
        "--cmd-log-size",
        "512",
        "--timer-format",
        "digital_compact",
        "--cmd-name-trunc",
        "10",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
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
                keyHide = KeyHideOn,
                pollInterval = 127,
                cmdLogSize = MkBytes 512,
                timerFormat = DigitalCompact,
                cmdNameTrunc = Just 10,
                cmdLogging =
                  Just
                    $ MkCmdLoggingP
                      { stripControl = StripControlNone,
                        lineTrunc = Just 60
                      },
                fileLogging =
                  Just
                    $ MkFileLoggingP
                      { path = FPManual logPath,
                        stripControl = StripControlNone,
                        mode = FileModeAppend,
                        sizeMode = Nothing
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
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeConfigAndAssertFieldEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI overrides config file cmd-log fields even when CLI --cmd-log is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
        "cmd"
      ]
    expected =
      [ #coreConfig % #cmdLogging %? #stripControl ^?=@ Just StripControlNone,
        #coreConfig % #cmdLogging %? #lineTrunc % _Just ^?=@ Just 60
      ]

cliOverridesConfigFileFileLog :: TestTree
cliOverridesConfigFileFileLog = testPropertyNamed desc "cliOverridesConfigFileFileLog"
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
        "--file-log-mode",
        "write",
        "--file-log-strip-control",
        "smart",
        "--file-log-size-mode",
        "warn 10 mb",
        "cmd"
      ]

    expected =
      [ #coreConfig % #fileLogging %? #stripControl ^?=@ Just StripControlSmart,
        #coreConfig % #fileLogging %? #mode ^?=@ Just FileModeWrite,
        #coreConfig % #fileLogging %? #sizeMode % _Just ^?=@ Just (FileSizeModeWarn $ MkBytes 10_000_000)
      ]

fileLogStripControlDefaultsAll :: TestTree
fileLogStripControlDefaultsAll = testPropertyNamed desc "fileLogStripControlDefaultsAll"
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
        "--no-key-hide",
        "--no-poll-interval",
        "--no-cmd-log-size",
        "--no-timer-format",
        "--no-cmd-name-trunc",
        "--no-cmd-log",
        "--no-cmd-log-strip-control",
        "--no-cmd-log-line-trunc",
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
        "--poll-interval",
        "555",
        "--no-poll-interval",
        "--cmd-log-size",
        "512",
        "--no-cmd-log-size",
        "--timer-format",
        "prose_full",
        "--no-timer-format",
        "--key-hide",
        "--no-key-hide",
        "--cmd-name-trunc",
        "80",
        "--no-cmd-name-trunc",
        "--cmd-log",
        "--no-cmd-log",
        "--cmd-log-strip-control",
        "all",
        "--no-cmd-log-strip-control",
        "--cmd-log-line-trunc",
        "100",
        "--no-cmd-log-line-trunc",
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
