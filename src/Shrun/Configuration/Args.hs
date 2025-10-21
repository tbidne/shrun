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
        edges
      ),
  )
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

defaultArgs :: NESeq Text -> Args
defaultArgs commands =
  MkArgs
    { configPaths = Empty,
      coreConfig = def,
      commands,
      edges = Nothing
    }
