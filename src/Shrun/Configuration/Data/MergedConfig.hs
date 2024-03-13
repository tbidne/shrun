{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
    defaultMergedConfig,
  )
where

import Shrun.Configuration.Data.Core
  ( CoreConfigMerged,
    CoreConfigP
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
import Shrun.Data.Command (CommandP1)
import Shrun.Data.KeyHide (defaultKeyHide)
import Shrun.Data.PollInterval (defaultPollInterval)
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
          { fileLogging = Nothing,
            cmdLogging = Nothing,
            timerFormat = defaultTimerFormat,
            pollInterval = defaultPollInterval,
            keyHide = defaultKeyHide,
            cmdNameTrunc = Nothing,
            cmdLogReadSize = defaultCmdLogReadSize,
            notify = Nothing,
            timeout = Nothing,
            init = Nothing
          },
      commands
    }
