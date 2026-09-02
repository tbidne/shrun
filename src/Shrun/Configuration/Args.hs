module Shrun.Configuration.Args
  ( Args (..),
    defaultArgs,
  )
where

import Shrun.Configuration.Args.Parsing
  ( Args
      ( MkArgs,
        commands,
        configPaths,
        coreConfig,
        dryRun,
        edges,
        expandAliases
      ),
  )
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

defaultArgs :: List Text -> Args m
defaultArgs commands =
  MkArgs
    { configPaths = Empty,
      coreConfig = def,
      commands,
      edges = Nothing,
      dryRun = False,
      expandAliases = False
    }
