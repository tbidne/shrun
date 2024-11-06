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

instance
  ( k ~ A_Lens,
    a ~ CoreConfigMerged,
    b ~ CoreConfigMerged
  ) =>
  LabelOptic "coreConfig" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f (MkMergedConfig a1 a2) ->
        fmap
          (\b -> MkMergedConfig b a2)
          (f a1)
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
      $ \f (MkMergedConfig a1 a2) ->
        fmap
          (\b -> MkMergedConfig a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

defaultMergedConfig :: NESeq CommandP1 -> MergedConfig
defaultMergedConfig commands =
  MkMergedConfig
    { coreConfig = CoreConfig.defaultCoreConfigMerged commands,
      commands
    }
