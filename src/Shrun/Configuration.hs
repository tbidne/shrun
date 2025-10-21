module Shrun.Configuration
  ( mergeConfig,
  )
where

import Shrun.Command.Types (CommandP (MkCommandP), fromPositive)
import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.Core (mergeCoreConfig)
import Shrun.Configuration.Data.Graph (EdgeArgs (EdgeArgsList))
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commandGraph,
        commands,
        coreConfig
      ),
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default qualified as D
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml (Toml)
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Merges Args and Toml together, filling in necessary defaults and
-- doing some light processing.
--
-- We want this function to do as much to prepare the final config as
-- possible. For instance, in addition to filling in defaults, we also process
-- commands via the legend (MonadThrow) and detect the terminal width for
-- command logging's lineTrunc field (MonadTerminal).
--
-- This is very nearly pure, except for the aforementioned effects.
-- The only remaining tasks the Env needs to take care of is IO that we
-- really can't test anyway, such as opening file handles and creating
-- queues.
mergeConfig ::
  ( HasCallStack,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Args ->
  Toml ->
  m MergedConfig
mergeConfig args toml = do
  (commands, ea) <- case toml ^. #legend of
    Nothing -> pure (mkCmd <$> cmdsTextIndexed, cliEdgeArgs)
    Just aliases -> do
      -- 3. We have a legend. Need to combine CLI and legend
      --    edges config. See NOTE: [CLI and Legend Edges]
      legendMap <- Legend.linesToMap aliases
      cmdEdges <- case wEdgeArgs of
        -- 3.1. We also have CLI edges; pass it in.
        Just (With ea) -> Legend.translateCommands legendMap cmdsText (Just ea)
        -- 3.2 No CLI legend; compute toml edges as normal.
        Nothing -> Legend.translateCommands legendMap cmdsText Nothing
        -- 3.3 Edges disabled; not only do we have no edges to pass
        -- in, we must also disable the toml edges. We do this
        -- but overwriting whatever was computed with 'def',
        -- below.
        Just Disabled -> do
          (cmds, _) <- Legend.translateCommands legendMap cmdsText Nothing
          pure (cmds, mempty)
      pure $ second EdgeArgsList cmdEdges

  coreConfig <-
    mergeCoreConfig
      commands
      (args ^. #coreConfig)
      (toml ^. #coreConfig)

  commandGraph <- Graph.mkGraph ea commands

  pure
    $ MkMergedConfig
      { coreConfig,
        commandGraph,
        commands
      }
  where
    cmdsText = args ^. #commands

    cmdsTextIndexed = Utils.indexPos (args ^. #commands)

    mkCmd (i, t) = MkCommandP (fromPositive i) Nothing t

    wEdgeArgs = args ^. #edges
    cliEdgeArgs = D.fromMaybe (wEdgeArgs >>= WD.toMaybe)
{-# INLINEABLE mergeConfig #-}
