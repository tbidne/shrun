{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
  )
where

import Shrun.Configuration.Data.Core (CoreConfigMerged)
import Shrun.Data.Legend (KeyVal)
import Shrun.Prelude

-- | Merged Args + Toml
data MergedConfig = MkMergedConfig
  { -- | Core config.
    coreConfig :: CoreConfigMerged,
    -- | Legend.
    legend :: Maybe (List KeyVal),
    -- | Commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MergedConfig
