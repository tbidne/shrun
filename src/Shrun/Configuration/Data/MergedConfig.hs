{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
    defaultMergedConfig,
  )
where

import Shrun.Configuration.Data.Core (CoreConfigMerged)
import Shrun.Configuration.Default (Default (def))
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

instance
  ( k ~ A_Lens,
    a ~ CoreConfigMerged,
    b ~ CoreConfigMerged
  ) =>
  LabelOptic "coreConfig" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkMergedConfig _coreConfig _commands) ->
          fmap
            (`MkMergedConfig` _commands)
            (f _coreConfig)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NESeq CommandP1,
    b ~ NESeq CommandP1
  ) =>
  LabelOptic "commands" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkMergedConfig _coreConfig _commands) ->
          fmap
            (MkMergedConfig _coreConfig)
            (f _commands)
  {-# INLINE labelOptic #-}

defaultMergedConfig :: NESeq CommandP1 -> MergedConfig
defaultMergedConfig commands =
  MkMergedConfig
    { coreConfig = def,
      commands
    }
