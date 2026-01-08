{-# LANGUAGE OverloadedLists #-}

module Shrun.Configuration.Args.Parsing.Graph.Utils
  ( -- * High-level
    MParser,
    parseIndexSet,
    parseOneIndex,

    -- * Errors
    FatalError (..),
    runFatalErrors,
    runFatalErrors1,

    -- * Combinators
    failIfNext,
    noLabel,
    optionalTry,
    parseIfNoComma,

    -- * Low-level
    char,
    string,
    lexeme,
    parseComma,
    parseDots,

    -- * Misc
    anyLeft,
    mkMpError,
  )
where

import Data.Char qualified as Ch
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set (Set)
import Shrun.Command.Types qualified as Cmd.T
import Shrun.Command.Types.Internal (CommandIndex (MkCommandIndex))
import Shrun.Prelude
import Text.Megaparsec
  ( ErrorFancy,
    ParseError (FancyError),
    Parsec,
    ShowErrorComponent,
    (<?>),
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Read qualified as TR

-- | Core parser type.
type MParser a = Parsec FatalError Text a

-- | Parses an index set.
--
-- @
--   - "{1, 2, 3}"
--   - "{1 .. 3}"
--   - "{1, 4..6}"
-- @
parseIndexSet :: MParser (NESeq CommandIndex)
parseIndexSet = do
  char '{'
  failIfNext "Empty set" (char '}')

  k <- parseElem
  ks <- many (parseComma *> parseElem)
  char '}'
  pure (join $ k :<|| listToSeq ks)
  where
    parseElem =
      runFatalError
        (Cmd.T.joinRange <$> parseRange)
        (NESeq.singleton <$> parseOneIndex)

-- | Parses a single index i.e. a positive integer.
parseOneIndex :: MParser CommandIndex
parseOneIndex = do
  txt <- lexeme $ MP.takeWhile1P (Just "digit") Ch.isDigit
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

-- | Parse "1..3".
parseRange :: MParser (Tuple2 CommandIndex (NESeq CommandIndex))
parseRange = do
  l <- parseOneIndex
  parseDots
  u <- parseOneIndex

  case Cmd.T.range l u of
    Right r -> pure r
    Left err -> MP.customFailure (MkFatalError err)

-- | Returns true iff eof is not succesfully parsed.
anyLeft :: MParser Bool
anyLeft = do
  MP.optional MP.eof <&> \case
    Nothing -> True
    Just () -> False

-- | Runs the parser if iff a comma is /not/ encountered. If a comma is
-- encountered, it is not consumed, and the default value is returned.
parseIfNoComma :: a -> MParser a -> MParser a
parseIfNoComma def p = lookAheadBranch parseComma p (pure def)

-- | Fails with the given error message if the given parser is next. Does not
-- consume the parser, for better carets in the error message.
failIfNext :: String -> MParser a -> MParser ()
failIfNext err p = lookAheadBranch p (pure ()) (fail err)

-- | @lookAheadBranch ptest p1 p2@ tries ptest without consuming it. If it
-- fails, runs p1. Otherwise runs p2.
--
-- Note if ptest fails, it may consume input. Consider try if this is a
-- problem.
lookAheadBranch :: MParser p -> MParser a -> MParser a -> MParser a
lookAheadBranch ptest p1 p2 =
  -- Clear label as we do not want it to interfere with error
  -- messages.
  MP.optional (noLabel $ MP.lookAhead ptest) >>= \case
    Nothing -> p1
    Just _ -> p2

-- | Parses a comma.
parseComma :: MParser ()
parseComma = char ',' $> ()

-- | Parses two dots.
parseDots :: MParser ()
parseDots = string ".." $> () <?> "range ('..')"

-- | Clears a parser label.
noLabel :: MParser a -> MParser a
noLabel = MP.label ""

-- | Optional + try i.e. returns Just iff the parser succeeds, otherwise
-- backtracks.
optionalTry :: MParser a -> MParser (Maybe a)
optionalTry = MP.optional . MP.try

-- | Modifies a parser to consume trailing whitespace.
lexeme :: MParser a -> MParser a
lexeme = Lex.lexeme MPC.space

-- | Consumes char and trailing whitespace.
char :: Char -> MParser Char
char = lexeme . MPC.char

-- | Consumes string and trailing whitespace.
string :: Text -> MParser Text
string = lexeme . MPC.string

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
runFatalErrors :: MParser a -> List (MParser a) -> MParser a
runFatalErrors = foldr runFatalError

-- | Like 'runFatalErrors' but with no default error. For when we want the
-- underlying error to be used.
runFatalErrors1 :: NonEmpty (MParser a) -> MParser a
runFatalErrors1 = foldr1 runFatalError

-- | Like '(<|>)', except it does not try the RHS when the LHS has a fatal
-- error.
runFatalError :: MParser a -> MParser a -> MParser a
runFatalError p1 p2 =
  MP.observing (MP.try p1) >>= \case
    Right x -> pure x
    Left err -> case hasFatalError err of
      Just errSet -> MP.fancyFailure errSet
      Nothing -> p2

hasFatalError :: ParseError s FatalError -> Maybe (Set (ErrorFancy FatalError))
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

mkMpError :: String -> String -> String
mkMpError u e =
  mconcat
    [ "unexpected ",
      u,
      "\nexpecting ",
      e
    ]
