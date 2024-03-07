module Shrun.Configuration.Args
  ( Args (..),
    defaultArgs,
  )
where

import Shrun.Configuration.Args.Parsing
  ( Args
      ( MkArgs,
        cmdLog,
        commands,
        configPath,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingP (MkCmdLoggingP, lineTrunc, stripControl),
  )
import Shrun.Configuration.Data.ConfigPhase
  ( WithDisable (MkWithDisable),
    emptyWithDisable,
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
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Prelude

defaultArgs :: NESeq Text -> Args
defaultArgs commands =
  MkArgs
    { configPath = emptyWithDisable,
      cmdLog = MkWithDisable (False, False),
      coreConfig =
        MkCoreConfigP
          { timeout = emptyWithDisable,
            init = emptyWithDisable,
            keyHide = emptyWithDisable,
            pollInterval = emptyWithDisable,
            cmdLogSize = emptyWithDisable,
            timerFormat = emptyWithDisable,
            cmdNameTrunc = emptyWithDisable,
            cmdLogging =
              MkCmdLoggingP
                { stripControl = emptyWithDisable,
                  lineTrunc = emptyWithDisable
                },
            fileLogging =
              MkFileLoggingP
                { path = emptyWithDisable,
                  stripControl = emptyWithDisable,
                  mode = emptyWithDisable,
                  sizeMode = emptyWithDisable
                },
            notify =
              MkNotifyP
                { action = emptyWithDisable,
                  system = emptyWithDisable,
                  timeout = emptyWithDisable
                }
          },
      commands
    }
