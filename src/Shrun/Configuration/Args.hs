module Shrun.Configuration.Args
  ( Args (..),
    defaultArgs,
  )
where

import Shrun.Configuration.Args.Parsing
  ( Args
      ( MkArgs,
        commands,
        configPath,
        coreConfig,
        edges
      ),
  )
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

defaultArgs :: NESeq Text -> Args
defaultArgs commands =
  MkArgs
    { configPath = [],
      coreConfig = def,
      commands,
      edges = Nothing
    }
