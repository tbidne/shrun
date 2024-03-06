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
import Shrun.Data.Command (CommandP1)
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.PollInterval (PollInterval (MkPollInterval))
import Shrun.Data.TimerFormat (TimerFormat (ProseCompact))
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
            timerFormat = ProseCompact,
            pollInterval = MkPollInterval 10_000,
            keyHide = KeyHideOff,
            cmdNameTrunc = Nothing,
            cmdLogSize = MkBytes 1024,
            notify = Nothing,
            timeout = Nothing,
            init = Nothing
          },
      commands
    }
