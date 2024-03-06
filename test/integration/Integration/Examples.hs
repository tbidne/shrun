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
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (MkMergedConfig, commands, coreConfig),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.TimerFormat (TimerFormat (ProseCompact))
import Shrun.Notify.Types
  ( NotifyAction (NotifyCommand),
    NotifySystem (AppleScript, NotifySend),
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
                keyHide = KeyHideOff,
                pollInterval = 100,
                cmdLogSize = MkBytes 2048,
                timerFormat = ProseCompact,
                cmdNameTrunc = Just 80,
                cmdLogging =
                  Just
                    $ MkCmdLoggingP
                      { stripControl = StripControlSmart,
                        lineTrunc = Just 150
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
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
        }

examplesDefault :: TestTree
examplesDefault = testPropertyNamed desc "examplesDefault"
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
