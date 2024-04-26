{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
    defaultMergedConfig,
  )
where

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
  ( ConsoleLoggingP
      ( MkConsoleLoggingP,
        cmdLogging,
        cmdNameTrunc,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigMerged,
    CoreConfigP
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
import Shrun.Data.Command (CommandP1)
import Shrun.Data.KeyHide (defaultKeyHide)
import Shrun.Data.PollInterval (defaultPollInterval)
import Shrun.Data.StripControl (defaultConsoleLogStripControl)
import Shrun.Data.TimerFormat (defaultTimerFormat)
import Shrun.Logging.Types (defaultCmdLogReadSize)
import Shrun.Prelude

-- | Merged Args + Toml
data MergedConfig = MkMergedConfig
  { -- | Core config.
    coreConfig :: CoreConfigMerged,
    -- | Commands.
    commands :: NESeq CommandP1
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MergedConfig

defaultMergedConfig :: NESeq CommandP1 -> MergedConfig
defaultMergedConfig commands =
  MkMergedConfig
    { coreConfig =
        MkCoreConfigP
          { commonLogging =
              MkCommonLoggingP
                { keyHide = defaultKeyHide,
                  timerFormat = defaultTimerFormat
                },
            cmdLogging =
              MkCmdLoggingP
                { pollInterval = defaultPollInterval,
                  readSize = defaultCmdLogReadSize
                },
            consoleLogging =
              MkConsoleLoggingP
                { cmdLogging = False,
                  cmdNameTrunc = Nothing,
                  lineTrunc = Nothing,
                  stripControl = defaultConsoleLogStripControl
                },
            fileLogging = Nothing,
            notify = Nothing,
            timeout = Nothing,
            init = Nothing
          },
      commands
    }
