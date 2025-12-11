{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Graph.Edges
  ( parseEdges,
  )
where

import Data.Sequence.NonEmpty qualified as NESeq
import Shrun.Command.Types qualified as Cmd.T
import Shrun.Command.Types.Internal (CommandIndex)
import Shrun.Configuration.Args.Parsing.Graph.Utils (MParser)
import Shrun.Configuration.Args.Parsing.Graph.Utils qualified as Utils
import Shrun.Configuration.Data.Graph
  ( Edge,
    EdgeLabel (EdgeAnd, EdgeAny, EdgeOr),
  )
import Shrun.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP

-- | Parses at least one edge, stops if any commas are found. We return
-- NESeq rather than Edges as the former allows us to be more precise that
-- at least one edge is required for a successful parse.
--
-- After we are done parsing all edges, we can combine them.
parseEdges :: MParser (NESeq Edge)
parseEdges = MP.label label parseEdges'
  where
    label =
      "comma-delimited edge(s) e.g. \"1 & 2, {3,4} ; 1, 4 &.. 6\""

parseEdges' :: MParser (NESeq Edge)
parseEdges' = MP.label label $ do
  Utils.optionalTry Utils.parseOneIndex >>= \case
    Just s -> parseEdgeIndex s
    Nothing -> Utils.parseIndexSet >>= parseEdgeIndexSet
  where
    label = "comma-delimited edge(s) e.g. \"1 & 2, {3,4} ; 1, 4 &.. 6\""

-- NOTE: [Commas]
--
-- Edges are delimited by commas e.g. '1 & 2, 2 & 3'. Hence if we see a comma
-- we want to stop (without consuming them!). We use the 'parseIfNoComma'
-- function to guard our parsers s.t. they do not run if they parse a comma.
--
-- The lone exception to this is the /first vertex/ i.e. we want to guarantee
-- at least one successful parse, hence a comma should be a failure.
--
-- To this, we split our parsers X into a normal variant (called first)
-- and an 'XComma' variant that stops if a comma is found. This allows us
-- to guarantee that if the parser succeeds, we have found at least one
-- value (hence non-empty type).

parseEdgeIndexComma :: CommandIndex -> MParser (Seq Edge)
parseEdgeIndexComma = Utils.parseIfNoComma [] . fmap neseqToSeq . parseEdgeIndex

parseEdgeIndex :: CommandIndex -> MParser (NESeq Edge)
parseEdgeIndex s = do
  -- Try dots for a better error message.
  MP.optional (Utils.noLabel Utils.parseDots) >>= \case
    Just () ->
      fail "Expected an edge, found '..'. Perhaps you wanted an edge range e.g. '&..'?"
    Nothing -> do
      lbl <- parseEdgeLabel
      MP.optional Utils.parseDots >>= \case
        Just () -> parseEdgeDots lbl s <?> label
        Nothing -> parseEdgeNoDots lbl (NESeq.singleton s) <?> label
  where
    label = "a vertex (e.g. '3', '{1,5}')"

parseEdgeIndexSetComma :: NESeq CommandIndex -> MParser (Seq Edge)
parseEdgeIndexSetComma = Utils.parseIfNoComma [] . fmap neseqToSeq . parseEdgeIndexSet

parseEdgeIndexSet :: NESeq CommandIndex -> MParser (NESeq Edge)
parseEdgeIndexSet idxs = do
  lbl <- parseEdgeLabel
  parseEdgeNoDots lbl idxs

parseEdgeDots :: EdgeLabel -> CommandIndex -> MParser (NESeq Edge)
parseEdgeDots lbl s = do
  d <- Utils.parseOneIndex
  (r1, r2 :<|| rs) <- case Cmd.T.range s d of
    Right r -> pure r
    Left err -> fail err

  let edges@(e1 :<|| es) = go (r2 :<| rs) (NESeq.singleton (r1, r2, lbl))
      go Empty acc = acc
      go (_ :<| Empty) acc = acc
      go (x1 :<| x2 :<| xs) acc = go (x2 :<| xs) (acc NESeq.|> (x1, x2, lbl))

  Utils.anyLeft >>= \case
    False -> pure edges
    True -> (\ds -> e1 :<|| es <> ds) <$> parseEdgeIndexComma d

parseEdgeNoDots :: EdgeLabel -> NESeq CommandIndex -> MParser (NESeq Edge)
parseEdgeNoDots lbl srcs = do
  mD <- Utils.optionalTry Utils.parseOneIndex
  (edges@(e1 :<|| es), parseMore) <- case mD of
    Just d -> do
      let edges = [(s, d, lbl) | s <- srcs]
      pure (edges, parseEdgeIndexComma d)
    Nothing -> do
      dests <- Utils.parseIndexSet
      let edges =
            [ (s, d, lbl)
            | s <- srcs,
              d <- dests
            ]
      pure (edges, parseEdgeIndexSetComma dests)

  Utils.anyLeft >>= \case
    False -> pure edges
    True -> (\ds -> e1 :<|| es <> ds) <$> parseMore

parseEdgeLabel :: MParser EdgeLabel
parseEdgeLabel = MP.label label $ do
  asum @List
    [ EdgeAnd <$ Utils.string "&",
      EdgeOr <$ Utils.string "|",
      EdgeAny <$ Utils.string ";"
    ]
  where
    label = "an edge ('&', '|', ';')"
