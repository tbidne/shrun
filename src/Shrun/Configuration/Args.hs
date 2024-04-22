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
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        cmdLogReadSize,
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
        cmdNameTrunc,
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
    { configPath = mempty,
      cmdLog = mempty,
      coreConfig =
        MkCoreConfigP
          { timeout = mempty,
            init = mempty,
            keyHide = mempty,
            pollInterval = mempty,
            cmdLogReadSize = mempty,
            timerFormat = mempty,
            cmdNameTrunc = mempty,
            cmdLogging =
              MkCmdLoggingP
                { stripControl = mempty,
                  lineTrunc = mempty
                },
            fileLogging =
              MkFileLoggingP
                { path = mempty,
                  cmdNameTrunc = mempty,
                  stripControl = mempty,
                  mode = mempty,
                  sizeMode = mempty
                },
            notify =
              MkNotifyP
                { action = mempty,
                  system = mempty,
                  timeout = mempty
                }
          },
      commands
    }
