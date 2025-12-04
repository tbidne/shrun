{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (..),
  )
where

import Shrun.Command.Types (CommandP1)
import Shrun.Configuration.Data.Core (CoreConfigMerged)
import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Prelude

-- | Merged Args + Toml
data MergedConfig = MkMergedConfig
  { -- | Core config.
    coreConfig :: CoreConfigMerged,
    -- | Command graph.
    commandGraph :: CommandGraph,
    -- | Commands.
    commands :: NESeq CommandP1,
    -- | Whether to print the config.
    dryRun :: Bool,
    -- | Toml paths used in this config.
    tomlPaths :: Seq OsPath
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
      $ \f (MkMergedConfig a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkMergedConfig b a2 a3 a4 a5)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ CommandGraph,
    b ~ CommandGraph
  ) =>
  LabelOptic "commandGraph" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f (MkMergedConfig a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkMergedConfig a1 b a3 a4 a5)
          (f a2)
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
      $ \f (MkMergedConfig a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkMergedConfig a1 a2 b a4 a5)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Bool,
    b ~ Bool
  ) =>
  LabelOptic "dryRun" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f (MkMergedConfig a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkMergedConfig a1 a2 a3 b a5)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Seq OsPath,
    b ~ Seq OsPath
  ) =>
  LabelOptic "tomlPaths" k MergedConfig MergedConfig a b
  where
  labelOptic =
    lensVL
      $ \f (MkMergedConfig a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkMergedConfig a1 a2 a3 a4 b)
          (f a5)
  {-# INLINE labelOptic #-}

instance Pretty MergedConfig where
  pretty c =
    vcat
      . toList
      $ prettyConfigPaths
      <> [ "config:",
           indentField $ pretty $ c ^. #coreConfig,
           "command-graph:",
           indentField $ pretty $ c ^. #commandGraph,
           "commands:",
           indentField prettyCommands
         ]
    where
      prettyCommands =
        vcat
          . toList
          . fmap prettyCommand
          $ c
          ^. #commands

      prettyCommand cmd =
        mconcat
          [ pretty (cmd ^. #index),
            ". ",
            pretty (cmd ^. #command)
          ]

      prettyConfigPaths = case c ^. #tomlPaths of
        Empty -> ["config-paths: off"]
        ps@(_ :<| _) ->
          "config-paths:"
            :<| (indentField . ("- " <>) . pretty . decodeLenient <$> ps)
