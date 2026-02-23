module Shrun.Configuration.Args.Parsing.Graph.Sequential
  ( parseSequential,
  )
where

import Shrun.Configuration.Args.Parsing.Graph.Utils (MParser)
import Shrun.Configuration.Args.Parsing.Graph.Utils qualified as Utils
import Shrun.Configuration.Data.Graph
  ( EdgeSequential (EdgeSequentialAnd, EdgeSequentialAny, EdgeSequentialOr),
  )
import Shrun.Prelude
import Text.Megaparsec qualified as MP

parseSequential :: MParser EdgeSequential
parseSequential = MP.label label $ do
  asum
    [ EdgeSequentialAnd <$ Utils.string "&&",
      EdgeSequentialOr <$ Utils.string "||",
      EdgeSequentialAny <$ Utils.string ";;"
    ]
  where
    label = "sequential literals (\"&&\", \"||\", \";;\")"
