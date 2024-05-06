{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils
  ( defaultConfig,
    makeConfigAndAssertEq,
    notifySystemOSDBus,
    notifySystemOSNotifySend,
    runConfigIO,
  )
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingP (MkCmdLoggingP, pollInterval, readSize),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP
      ( MkCommonLoggingP,
        keyHide,
        timerFormat
      ),
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
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (MkMergedConfig, commands, coreConfig),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Data.CmdLogReadSize (CmdLogReadSize (MkCmdLogReadSize))
import Shrun.Data.Command (CommandP (MkCommandP))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.TimerFormat (TimerFormat (ProseCompact))
import Shrun.Notify.Types
  ( NotifyAction (NotifyCommand),
    NotifySystemP (AppleScript, NotifySend),
    NotifyTimeout (NotifyTimeoutNever),
  )

specs :: TestTree
specs =
  testGroup
    "Examples"
    [ examplesConfig,
      examplesDefault
    ]

examplesConfig :: TestTree
examplesConfig = testPropertyNamed desc "examplesConfig"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "examples/config.toml is valid"
    args = ["-c", getExampleConfigOS "config", "cmd1"]
    expected =
      MkMergedConfig
        { coreConfig =
            MkCoreConfigP
              { timeout = Just 20,
                init = Just ". examples/bashrc",
                commonLogging =
                  MkCommonLoggingP
                    { keyHide = KeyHideOff,
                      timerFormat = ProseCompact
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { cmdLogging = ConsoleLogCmdOn,
                      cmdNameTrunc = Just 80,
                      lineTrunc = Just 150,
                      stripControl = StripControlSmart
                    },
                cmdLogging =
                  MkCmdLoggingP
                    { pollInterval = 100,
                      readSize = MkCmdLogReadSize $ MkBytes 2048
                    },
                fileLogging = Nothing,
                notify =
                  Just
                    $ MkNotifyP
                      { action = NotifyCommand,
                        system = notifySystemOSNotifySend,
                        timeout = NotifyTimeoutNever
                      }
              },
          commands = MkCommandP (Just "cmd1") "echo \"command one\"" :<|| []
        }

examplesDefault :: TestTree
examplesDefault = testPropertyNamed desc "examplesDefault"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "examples/default.toml is valid"
    args = ["-c", getExampleConfig "default", "cmd"]
    expected = defaultConfig
