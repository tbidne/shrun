{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
    defaultMergedConfig,
  )
where

import Shrun.Configuration.Data.Core (CoreConfigMerged)
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Data.Command (CommandP1)
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
    { coreConfig = CoreConfig.defaultMerged,
      commands
    }
