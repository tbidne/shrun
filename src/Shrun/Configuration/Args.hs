module Shrun.Configuration.Args
  ( Args (..),
    defaultArgs,
  )
where

import Shrun.Configuration.Args.Parsing
  ( Args
      ( MkArgs,
        commands,
        configPath,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.CommandLogging
  ( CommandLoggingP (MkCommandLoggingP, pollInterval, readSize),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP (MkCommonLoggingP, keyHide, timerFormat),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl
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
  ( FileLogInitP
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
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Prelude

defaultArgs :: NESeq Text -> Args
defaultArgs commands =
  MkArgs
    { configPath = mempty,
      coreConfig =
        MkCoreConfigP
          { timeout = mempty,
            init = mempty,
            commonLogging =
              MkCommonLoggingP
                { keyHide = mempty,
                  timerFormat = mempty
                },
            consoleLogging =
              MkConsoleLoggingP
                { commandLogging = mempty,
                  commandNameTrunc = mempty,
                  lineTrunc = mempty,
                  stripControl = mempty
                },
            commandLogging =
              MkCommandLoggingP
                { pollInterval = mempty,
                  readSize = mempty
                },
            fileLogging =
              MkFileLoggingP
                { file =
                    MkFileLogInitP
                      { path = mempty,
                        mode = mempty,
                        sizeMode = mempty
                      },
                  commandNameTrunc = mempty,
                  deleteOnSuccess = mempty,
                  lineTrunc = mempty,
                  stripControl = mempty
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
