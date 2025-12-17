{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Graph
  ( edgesParser,
    parseEdges,
  )
where

import Data.Text qualified as T
import Options.Applicative (Parser, ReadM)
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Shrun.Configuration.Args.Parsing.Graph.Edges qualified as Edges
import Shrun.Configuration.Args.Parsing.Graph.Sequential qualified as Sequential
import Shrun.Configuration.Args.Parsing.Graph.Utils (MParser)
import Shrun.Configuration.Args.Parsing.Graph.Utils qualified as Utils
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Graph
  ( EdgeArgs
      ( EdgeArgsList,
        EdgeArgsSequential
      ),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import Text.Megaparsec qualified as MP

edgesParser :: Parser (Maybe (WithDisabled EdgeArgs))
edgesParser =
  Utils.mWithDisabledParser
    readEdges
    opts
    "EDGES_STR | &&& | ||| | ;;;"
  where
    opts =
      [ OA.long "edges",
        helpTxt
      ]

    helpTxt =
      OA.helpDoc
        . Chunk.unChunk
        . fmap (<> Pretty.line)
        . Chunk.vsepChunks
        $ [ items,
            outro
          ]

    items =
      Utils.itemizeHelper
        $ intro
        :<|| [ andEdges,
               orEdges,
               anyEdges
             ]

    intro =
      mconcat
        [ "Comma-separated list, specifying command dependencies, based on ",
          "their left-to-right order. There are three edge types:"
        ]

    andEdges = "and: '1 & 2', runs cmd2 iff cmd1 succeeds."
    orEdges = "or: '1 | 2', runs cmd2 iff cmd1 fails."
    anyEdges = "any: '1 ; 2', runs cmd2 iff cmd1 finishes."

    outro =
      Chunk.paragraph
        $ mconcat
          [ "The literals are equivalent to placing edges between all ",
            "commands e.g. '&&&' puts an 'and'-edge between all commands."
          ]

readEdges :: ReadM EdgeArgs
readEdges = do
  txt <- OA.str
  case parseEdges txt of
    Left err -> fail err
    Right cga -> pure cga

parseEdges :: Text -> Either String EdgeArgs
parseEdges txt = do
  let stripped = T.stripStart txt
  when (T.null stripped) $ do
    Left $ "Received empty input: '" ++ unpack txt ++ "'"

  first MP.errorBundlePretty
    $ MP.parse (parser <* MP.eof) "" stripped

parser :: MParser EdgeArgs
parser = do
  asum @List
    [ EdgeArgsSequential <$> Sequential.parseSequential,
      EdgeArgsList <$> parseMultiEdges
    ]

parseMultiEdges :: MParser Edges
parseMultiEdges = do
  e <- p
  es <- MP.many (Utils.parseComma *> p)
  pure $ MkEdges . neseqToSeq $ fold1 (e :| es)
  where
    p = Edges.parseEdges
