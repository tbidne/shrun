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
      ( ReadBlock
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
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( TimerFormat (ProseCompact),
  )
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
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Data.LegendKeysCache
  ( LegendKeysCache
      ( LegendKeysAdd,
        LegendKeysWrite
      ),
  )
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commandGraph,
        commands,
        coreConfig,
        dryRun,
        tomlPaths
      ),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyActionsActive (NotifyActionsActiveAll),
    NotifyP (MkNotifyP, actions, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyActionComplete (NotifyActionCompleteCommand),
    NotifyActionStartSwitch (MkNotifyActionStartSwitch),
  )
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (AppleScript, NotifySend),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever),
  )
import Shrun.Configuration.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))

specs :: TestTree
specs =
  testGroup
    "Examples"
    [ examplesConfig
    ]

examplesConfig :: TestTree
examplesConfig = testProp1 desc "examplesConfig" $ do
  logsRef <- liftIO $ newIORef' []
  makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

  logs <- liftIO $ readIORef' logsRef
  [] === logs
  where
    desc = "examples/config.toml is valid"
    args = ["-c", "off", "-c", getExampleConfigOS, "cmd1"]
    expected =
      MkMergedConfig
        { coreConfig =
            MkCoreConfigP
              { timeout = With 20,
                init = Just ". examples/bashrc",
                legendKeysCache = LegendKeysWrite,
                commonLogging =
                  MkCommonLoggingP
                    { debug = MkDebug False,
                      keyHide = MkKeyHideSwitch False
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { commandLogging = MkConsoleLogCmdSwitch True,
                      commandNameTrunc = Just 80,
                      lineTrunc = Just 150,
                      stripControl = StripControlSmart,
                      timerFormat = ProseCompact
                    },
                commandLogging =
                  MkCommandLoggingP
                    { bufferLength = 2000,
                      bufferTimeout = fromℤ 60,
                      pollInterval = 100,
                      readSize = MkReadSize $ MkBytes 1_000_000,
                      readStrategy = ReadBlock,
                      reportReadErrors = MkReportReadErrorsSwitch False
                    },
                fileLogging = Nothing,
                notify =
                  Just
                    $ MkNotifyP
                      { actions = NotifyActionsActiveAll NotifyActionCompleteCommand,
                        system = notifySystemOSNotifySend,
                        timeout = NotifyTimeoutNever
                      }
              },
          commandGraph = Graph.mkEdgelessGraph commands,
          commands,
          dryRun = False,
          tomlPaths = [getExampleConfigPathOS]
        }
    commands = MkCommandP (mkIdx 1) (Just "cmd1") "echo \"command one\"" :<|| []
