{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
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
data MergedConfig notifyEnv = MkMergedConfig
  { -- | Core config.
    coreConfig :: CoreConfigMerged notifyEnv,
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

makeFieldLabelsNoPrefix ''MergedConfig

instance Pretty (MergedConfig r) where
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
