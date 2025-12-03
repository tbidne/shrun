{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Graph
  ( edgesParser,
    parseEdges,
  )
where

import Data.Char qualified as Ch
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Options.Applicative (Parser, ReadM)
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Shrun.Command.Types.Internal (CommandIndex (MkCommandIndex), range)
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Graph
  ( Edge,
    EdgeArgs
      ( EdgeArgsList,
        EdgeArgsSequential
      ),
    EdgeLabel (EdgeAnd, EdgeAny, EdgeOr),
    EdgeSequential (EdgeSequentialAnd, EdgeSequentialAny, EdgeSequentialOr),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import Text.Megaparsec
  ( ErrorFancy,
    ParseError (FancyError),
    Parsec,
    ShowErrorComponent,
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Read qualified as TR

edgesParser :: Parser (Maybe (WithDisabled EdgeArgs))
edgesParser =
  Utils.mWithDisabledParser
    readEdges
    opts
    "EDGES_STR | seq_and | seq_or | seq_any"
  where
    opts =
      [ OA.long "edges",
        OA.completeWith ["seq_and", "seq_or", "seq_any"],
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
          "their order. There are three edge types:"
        ]

    andEdges = "and: 'cmd1 & cmd2', runs cmd2 iff cmd1 succeeds."
    orEdges = "or: 'cmd1 | cmd2', runs cmd2 iff cmd1 fails."
    anyEdges = "any: 'cmd1 ; cmd2', runs cmd2 iff cmd1 finishes."

    outro =
      Chunk.paragraph
        $ mconcat
          [ "The literals are equivalent to placing edges between all ",
            "commands e.g. 'seq_and' puts an 'and'-edge between all commands."
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
  if T.null stripped
    then Left $ "Received empty input: '" ++ unpack txt ++ "'"
    -- eof so that trailing charts that failed to parse cause an error.
    else case MP.parse (p <* MP.eof) "" stripped of
      Left err -> Left $ MP.errorBundlePretty err
      Right cga -> pure cga
  where
    p =
      asum @List
        [ MP.try parseSeqAnd,
          MP.try parseSeqOr,
          MP.try parseSeqAny,
          parseExtendedEdges
        ]

type MParser a = Parsec FatalError Text a

parseSeqAnd :: MParser EdgeArgs
parseSeqAnd = MPC.string "seq_and" $> EdgeArgsSequential EdgeSequentialAnd

parseSeqOr :: MParser EdgeArgs
parseSeqOr = MPC.string "seq_or" $> EdgeArgsSequential EdgeSequentialOr

parseSeqAny :: MParser EdgeArgs
parseSeqAny = MPC.string "seq_any" $> EdgeArgsSequential EdgeSequentialAny

-- | Parses at least one (extended-)edge. Examples with the edge results:
--
-- @
-- 1 & 2
--   - (1, 2)
--
-- 1 & 2 & 3:
--   - (1, 2)
--   - (2, 3)
--
-- {1, 2} & {3, 4} & {5, 6}:
--   - (1, 3)
--   - (1, 4)
--   - (2, 3)
--   - (2, 4)
--   - (3, 5)
--   - (3, 6)
--   - (4, 5)
--   - (4, 6)
-- @
parseExtendedEdges :: MParser EdgeArgs
parseExtendedEdges = do
  -- NOTE: If the user specifies --edges we require that there is at
  -- least one edge (or 'sequential'). Note that this is _not_ reflected in
  -- the type, as we use the edges monoid instance (empty list) to reflect
  -- the normal case: No edges means all commands are run concurrently.
  e1 <- parseOneExtendedEdge
  -- zero or more ', <edge>'
  es <- many (parseComma *> parseOneExtendedEdge)
  pure $ EdgeArgsList $ MkEdges $ join (neseqToSeq e1 :<| fmap neseqToSeq (listToSeq es))

-- | A single graph token.
data GraphToken
  = -- | A set of indices representing a single "node", though multiple
    -- indices represents multiple vertices. Examples:
    --
    -- - '1'
    -- - '{1}
    -- - '{1, 3..5}'
    GraphIndices (NESeq CommandIndex)
  | -- | An arrow between edges i.e. '&'
    GraphArrow GraphArrowToken
  deriving stock (Show)

data GraphArrowToken
  = GraphArrowAnd
  | GraphArrowOr
  | GraphArrowAny
  deriving stock (Show)

-- | Parses one (possibly extended) edge. That is, we allow > 1 arrows i.e.
--
-- @
--   <indices> & <indices>
--   <indices> & <indices> & <indices> & ...
-- @
--
-- where @<indices>@ can be either a single index or an index set.
parseOneExtendedEdge :: MParser (NESeq Edge)
parseOneExtendedEdge = lexeme $ do
  -- Some index (set) e.g. '1', '{1,3..5}', '2&..4'.
  node <- parseNode
  -- Possible edge destination(s) e.g. '& 3', '& {3, 4..6}', '& 2&..4'.
  -- This is 'many' (zero or more) not 'some' (one or more) because node
  -- can actually be self-contained, if it is an arrow range
  -- (e.g. '1&..3').
  nodes <- many parseEdgeDest
  let tokens = neseqToSeq node <> (listToSeq nodes >>= neseqToSeq)
  mkEdges tokens >>= \case
    Empty -> fail "Empty edges"
    (e :<| es) -> pure $ e :<|| es
  where
    -- Create edges between consecutive token sets.
    mkEdges :: Seq GraphToken -> MParser (Seq Edge)
    mkEdges tokens = go tokens
      where
        go :: Seq GraphToken -> MParser (Seq Edge)
        -- A single node: fine, presumably the end.
        go (GraphIndices _ :<| Empty) = pure Empty
        -- source -> dest; make edge(s).
        go (GraphIndices s :<| GraphArrow arrow :<| GraphIndices d :<| rest) =
          let lbl = case arrow of
                GraphArrowAnd -> EdgeAnd
                GraphArrowOr -> EdgeOr
                GraphArrowAny -> EdgeAny
           in (neseqToSeq (cartProd lbl s d) <>) <$> go (GraphIndices d :<| rest)
        go rest =
          fail
            $ mconcat
              [ "Expected '<indices> <arrow> <indices>' syntax, found: '",
                render rest,
                "'.\nFull token list: ",
                render tokens
              ]

    cartProd :: EdgeLabel -> NESeq CommandIndex -> NESeq CommandIndex -> NESeq Edge
    cartProd lbl srcs dests = [(s, d, lbl) | s <- srcs, d <- dests]

    render :: Seq GraphToken -> String
    render =
      unpack
        . T.intercalate " "
        . toList
        . fmap renderToken

    renderToken (GraphArrow GraphArrowAnd) = "&"
    renderToken (GraphArrow GraphArrowOr) = "|"
    renderToken (GraphArrow GraphArrowAny) = ";"
    renderToken (GraphIndices idxs) = renderIndices idxs

    renderIndices =
      (\t -> "<" <> t <> "> ")
        . T.intercalate ","
        . fmap (showt . view (#unCommandIndex % #unPositive))
        . toList

-- | Parses an "edge destination" i.e. a leading arrow and node: "& <node>".
parseEdgeDest :: MParser (NESeq GraphToken)
parseEdgeDest = do
  arrow <- parseArrow
  (\(t :<|| ts) -> GraphArrow arrow :<|| (t :<| ts)) <$> parseNode

-- | Parses an arrow token.
parseArrow :: MParser GraphArrowToken
parseArrow =
  lexeme
    $ asum @List
      [ GraphArrowAnd <$ MPC.string "&",
        GraphArrowOr <$ MPC.string "|",
        GraphArrowAny <$ MPC.string ";"
      ]

-- | Parses a single "node", where node is a single string "entity", but the
-- entity could expand to multiple tokens.
--
-- @
--   - "1"          => 1
--   - "{2, 4..6}"" => {2, 4, 5, 6}
--   - "1&..3"       => 1 & 2 & 3
-- @
parseNode :: MParser (NESeq GraphToken)
parseNode = do
  runFatalErrors
    pDef
    [ NESeq.singleton <$> parseIndexSet,
      parseArrowRange,
      NESeq.singleton . GraphIndices . NESeq.singleton <$> parseOneIndex
    ]
  where
    pDef =
      fail
        "Expected a set, arrow range, or index. Examples: '{1,2}', '1 &.. 3', '1'."

-- | Parses an index set.
--
-- @
--   - "{1, 2, 3}"
--   - "{1 .. 3}"
--   - "{1, 4..6}"
-- @
parseIndexSet :: MParser GraphToken
parseIndexSet = lexeme $ do
  lexeme $ MPC.char '{'
  MP.optional (MPC.char '}') >>= \case
    Nothing -> pure ()
    Just _ -> MP.customFailure (MkFatalError "Empty set")

  k <- parseElem
  ks <- many (parseComma *> parseElem)
  MPC.char '}'
  pure (GraphIndices $ join $ k :<|| listToSeq ks)
  where
    parseElem = lexeme $ runFatalError parseRange (NESeq.singleton <$> parseOneIndex)

-- | Parses an "arrow range" i.e. a range that represents an extended edge.
--
-- @
--   "1&..3" <=> "1 & 2 & 3"
-- @
parseArrowRange :: MParser (NESeq GraphToken)
parseArrowRange =
  asum @List
    [ MP.try $ toTokens GraphArrowAnd <$> parseRangeLit (Just "&.."),
      MP.try $ toTokens GraphArrowOr <$> parseRangeLit (Just "|.."),
      toTokens GraphArrowAny <$> parseRangeLit (Just ";..")
    ]
  where
    toTokens arr =
      NESeq.intersperse (GraphArrow arr)
        . fmap (GraphIndices . NESeq.singleton)

-- | Parse "1..3".
parseRange :: MParser (NESeq CommandIndex)
parseRange = parseRangeLit Nothing

-- | Parse "1<lit>3" e.g. "1 .. 3", "1 &.. 3".
parseRangeLit :: Maybe Text -> MParser (NESeq CommandIndex)
parseRangeLit mLit = lexeme $ do
  l <- parseOneIndex
  u <- case mLit of
    Nothing -> do
      _ <- lexeme $ MPC.string ".."
      parseOneIndex
    Just lit -> do
      -- If we are parsing a custom literal, than '..' is an error. Try to
      -- parse it here anyway, so we can give a better error.
      MP.optional (MPC.string "..") >>= \case
        Nothing -> pure ()
        Just _ -> do
          let err =
                mconcat
                  [ "Found '..' in extended range syntax. Perhaps you wanted ",
                    "e.g. '&..'?"
                  ]
          MP.customFailure (MkFatalError err)

      _ <- lexeme $ MPC.string lit
      parseOneIndex

  case range l u of
    Right r -> pure r
    Left err -> MP.customFailure (MkFatalError err)

-- | Parses a single index i.e. a positive integer.
parseOneIndex :: MParser CommandIndex
parseOneIndex = lexeme $ do
  txt <- MP.takeWhile1P (Just "digit") Ch.isDigit
  case TR.readMaybe @Int (unpack txt) of
    Just n -> case mkPositive n of
      Right p -> pure $ MkCommandIndex p
      Left err -> fail err
    Nothing ->
      fail
        $ mconcat
          [ "Failed parsing nat: ",
            unpack txt
          ]

parseComma :: MParser Char
parseComma = lexeme $ MPC.char ','

lexeme :: MParser a -> MParser a
lexeme = Lex.lexeme MPC.space

-- | Represents a "fatal" parse error i.e. we should not try any other parsers.
-- This exists for better error messages.
--
-- Suppose we try parsers p1 and p2 on some text. If p1 fails, normally we will
-- try p2, and if /that/ fails, use its error message. However, it may be the
-- case that p1 has the "real" failure, and its error message would be
-- better.
--
-- For example, when parsing nodes, we attempt to parse, in order:
--
--   - Sets: @"{2,3}"@
--   - Arrow ranges: @"2..3"@
--   - Positive integers: @"3"@
--
-- Suppose we parse '{3..2}'. This is a set, but the set parser will fail due
-- to a bad range error. Normally, we will then try arrow ranges and positive
-- integers, which will both fail, and the positive integer error will be
-- reported. This is misleading though, as the real problem was deep in the
-- set parser i.e. the range.
--
-- Hence we report this as a "fatal error", and have special logic that only
-- tries other parsers when no fatal errors have been encountered.
newtype FatalError = MkFatalError String
  deriving stock (Eq, Ord, Show)

instance ShowErrorComponent FatalError where
  showErrorComponent (MkFatalError s) = s

-- | Like 'asum', except the combinator is 'runFatalError'. The first
-- parser is the default when all fail, intended for a better error message.
runFatalErrors :: forall a. MParser a -> List (MParser a) -> MParser a
runFatalErrors = foldr runFatalError

-- | Like '(<|>)', except it does not try the RHS when the LHS has a fatal
-- error.
runFatalError :: MParser a -> MParser a -> MParser a
runFatalError p1 p2 =
  MP.observing (MP.try p1) >>= \case
    Right x -> pure x
    Left err -> case hasFatalError err of
      Just errSet -> MP.fancyFailure errSet
      Nothing -> p2

hasFatalError :: ParseError Text FatalError -> Maybe (Set (ErrorFancy FatalError))
hasFatalError = \case
  FancyError _ errSet ->
    -- NOTE: We can improve this from a linear search to O(log n) by making
    -- FatalError's Eq trivial (always True), and doing a lookup for
    -- 'MkFatalError ""'. This function would then return Bool instead of
    -- the error, and 'runFatalError' would return 'p1' instead of
    -- restoring the error (unnecessary regardless).
    --
    -- This has a slight change on the test output e.g. the error
    --
    --    option --edges: 1:6:
    --      |
    --    1 | 1 & 3..2
    --      |      ^
    --   Bad range. Expected 3 <= 2
    --
    -- Becomes
    --
    --    option --edges: 1:10:
    --      |
    --    1 | 1 & 3..2
    --      |          ^
    --   Bad range. Expected 3 <= 2
    --
    -- i.e. the caret indicates that the range was consumed. This is
    -- probably what we want anyway, though we put it off for now as this
    -- "optimization" is likely useless (errSet has max size 1, anyway),
    -- and reducing Eq/Ord may be undesirable for other reasons.
    if any k errSet
      then Just errSet
      else Nothing
  _ -> Nothing
  where
    k = \case
      MP.ErrorCustom _ -> True
      _ -> False
