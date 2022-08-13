{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities.
--
-- @since 0.1
module Shrun.Utils
  ( -- * Text Utils
    breakStripPoint,
    decodeUtf8Lenient,
    splitOn,
    truncateIfNeeded,
    stripControlAll,
    stripControlSmart,

    -- * Timing Utils
    diffTime,
    foldMap1,

    -- * Misc Utils
    parseByteText,
  )
where

import Data.Bytes
import Data.Char (isControl, isLetter)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncErr
import GHC.Exts (IsList (..))
import GHC.Int (Int64)
import Refined qualified as R
import Shrun.Data.NonEmptySeq (NonEmptySeq (..))
import Shrun.Prelude
import System.Clock (TimeSpec (..))
import System.Clock qualified as C

-- $setup
-- >>> :set -XOverloadedLists
-- >>> :set -XTemplateHaskell
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Data.Semigroup (Sum (..))

-- | For given \(x, y\), returns the absolute difference \(|x - y|\)
-- in seconds.
--
-- ==== __Examples__
-- >>> :{
--   let t1 = TimeSpec 5 0
--       -- 20 s + 1 billion ns = 21 s
--       t2 = TimeSpec 20 1_000_000_000
--   in diffTime t1 t2
-- :}
-- 16
--
-- @since 0.1
diffTime :: TimeSpec -> TimeSpec -> Natural
diffTime t1 t2 = i642n $ C.sec $ C.diffTimeSpec t1 t2
  where
    -- Allegedly safe because 'C.diffTimeSpec' guaranteed to be non-negative.
    i642n :: Int64 -> Natural
    i642n = fromIntegral
{-# INLINEABLE diffTime #-}

-- | Relaxes 'foldMap'\'s 'Monoid' constraint to 'Semigroup'. Requires a
-- starting value. This will have to do until semigroupoids' Foldable1 is
-- in base.
--
-- ==== __Examples__
-- >>> foldMap1 @List Sum 0 [1..4]
-- Sum {getSum = 10}
--
-- >>> -- Silly, but demonstrates usage i.e. with non-monoid NonEmpty.
-- >>> foldMap1 @List (:| []) 1 [2,3,4]
-- 1 :| [2,3,4]
--
-- @since 0.1
foldMap1 :: (Foldable f, Semigroup s) => (a -> s) -> a -> f a -> s
foldMap1 f x xs = foldr (\b g y -> f y <> g b) f xs x
{-# INLINEABLE foldMap1 #-}

-- | Wrapper for 'Text'\'s 'T.breakOn' that differs in two ways:
--
-- 1. Total, since we restrict the @needle@ to 'R.NonEmpty'
-- ('Text' throws a pure exception here).
-- 2. If the @needle@ is found within the @haystack@, we do not include it
-- in the second part of the pair.
--
-- ==== __Examples__
-- >>> let equals = $$(R.refineTH @R.NonEmpty @Text "=")
--
-- >>> -- Data.Text
-- >>> T.breakOn "=" "HEY=LISTEN"
-- ("HEY","=LISTEN")
--
-- >>> -- Shrun.Utils.Text
-- >>> breakStripPoint equals "HEY=LISTEN"
-- ("HEY","LISTEN")
--
-- Other examples:
--
-- >>> breakStripPoint equals "HEYLISTEN"
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint equals "=HEYLISTEN"
-- ("","HEYLISTEN")
--
-- >>> breakStripPoint equals "HEYLISTEN="
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint equals "HEY==LISTEN"
-- ("HEY","=LISTEN")
--
-- @since 0.1
breakStripPoint :: Refined R.NonEmpty Text -> Text -> Tuple2 Text Text
breakStripPoint rpoint txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair
  where
    point = R.unrefine rpoint
{-# INLINEABLE breakStripPoint #-}

-- | Decodes a 'ByteString' to UTF-8 in lenient mode.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncErr.lenientDecode
{-# INLINEABLE decodeUtf8Lenient #-}

-- | Wrapper around "Text"\'s 'T.splitOn'. This /should/ be total, as
-- 'T.splitOn' is partial exactly when the first parameter is empty, which we
-- reject. Unfortunately we have to perform a partial pattern match on the
-- result since 'T.splitOn' returns a list, even though the result should
-- always be 'NonEmptySeq'. Hence 'HasCallStack'.
--
-- ==== __Examples__
-- >>> splitOn $$(R.refineTH ",,") "hey,,listen"
-- "hey" :|^ fromList ["listen"]
--
-- >>> splitOn $$(R.refineTH ",,") "heylisten"
-- "heylisten" :|^ fromList []
--
-- >>> splitOn $$(R.refineTH ",") ""
-- "" :|^ fromList []
--
-- @since 0.1
splitOn :: HasCallStack => Refined R.NonEmpty Text -> Text -> NonEmptySeq Text
splitOn rs txt = case T.splitOn s txt of
  [] -> error err
  (t : ts) -> t :|^ fromList ts
  where
    err =
      "[Shrun.Utils] Impossible: Text.splitOn returned empty list. Split: "
        <> s
        <> ", text: "
        <> txt
    s = R.unrefine rs
{-# INLINEABLE splitOn #-}

-- | For 'Natural' \(n\) and 'Text' \(t = t_0 t_1 \ldots t_m\), truncates
-- \(t\) if \(m > n\). In this case, \(t\) is truncated to \(n - 3\), and an
-- ellipsis ( \(\ldots\) ) is appended. We are left with a string with
-- length exactly \(n\):
--
-- \[
-- t_0 t_1 \ldots t_{n-3} \text{...} \quad \text{-- 3 literal } `\text{.' chars appended}
-- \]
--
-- ==== __Examples__
-- >>> truncateIfNeeded 7 "hi"
-- "hi"
--
-- >>> truncateIfNeeded 10 "This is 21 chars long"
-- "This is..."
--
-- @since 0.1
truncateIfNeeded :: Natural -> Text -> Text
truncateIfNeeded n txt
  | T.length txt <= n' = txt
  | otherwise = txt'
  where
    txt' = T.take (n' - 3) txt <> "..."
    n' = n2i n
{-# INLINEABLE truncateIfNeeded #-}

n2i :: Natural -> Int
n2i = fromIntegral
{-# INLINEABLE n2i #-}

-- | Strips all control chars, including ansi escape sequences. Leading
-- and trailing whitespace is also stripped.
--
-- ==== __Examples__
--
-- >>> stripControlAll "foo\ESC[0;3Abar \n baz"
-- "foobar  baz"
--
-- @since 0.5
stripControlAll :: Text -> Text
stripControlAll =
  -- The ansi stripping must come first. For example, if we strip control
  -- chars from "\ESC[0;3mfoo" we get "0;3mfoo", and then stripAnsiAll will
  -- no longer recognize this as an ansi sequences - i.e. this will leave
  -- remnants from the ansi sequences.
  --
  -- By performing stripAnsiAll first, we remove entire ansi sequences,
  -- then remove other control chars (e.g. newlines, tabs).
  T.strip . T.filter (not . isControl) . stripAnsiAll

-- | Strips control chars, including most ansi escape sequences. Leading and
-- trailing whitespace is also stripped. We leave behind SGR ansi escape
-- sequences e.g. text coloring. See
-- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters.
--
-- ==== __Examples__
--
-- >>> stripControlSmart "foo\ESC[0;3Abar \n baz"
-- "foobar  baz"
--
-- >>> stripControlSmart "foo\ESC[0;3mbar \n baz"
-- "foo\ESC[0;3mbar  baz"
--
-- @since 0.5
stripControlSmart :: Text -> Text
stripControlSmart =
  -- Like 'stripControlAll', we need to handle the ansi sequences first.
  -- Because we actually leave some sequences behind, we need to be more
  -- surgical removing the rest of the control chars (e.g. newline, tabs).
  T.strip . T.filter ctrlToFilter . stripAnsiControl
  where
    -- stripAnsiControl should be handling all \ESC sequences, so we should
    -- be safe to ignore these, accomplishing our goal of preserving the SGR
    -- sequences. If this is too aggressive, we can instead attempt to strip
    -- out the known 'bad' control chars e.g.
    --
    --   ctrlToFilter = not . (`elem` ['\n', '\t', '\v'])
    --
    ctrlToFilter c
      | isControl c = c == '\ESC'
      | otherwise = True

-- | Strips all ansi sequences from the given text.
--
-- ==== __Examples__
--
-- >>> stripAnsiAll "foo\ESC[0;3Abar"
-- "foobar"
--
-- @since 0.5
stripAnsiAll :: Text -> Text
stripAnsiAll = T.concat . fmap (view _1) . splitAnsi

-- | Strips ansi control sequences only.
--
-- ==== __Examples__
--
-- >>> stripAnsiControl "foo\ESC[0;3Abar"
-- "foobar"
--
-- >>> stripAnsiControl "foo\ESC[0;3mbar"
-- "foo\ESC[0;3mbar"
--
-- @since 0.5
stripAnsiControl :: Text -> Text
stripAnsiControl txt =
  foldl' f "" splitTxt
  where
    splitTxt = splitAnsi txt
    f acc (preAnsi, code, withAnsi)
      | nonControlAnsi code = acc <> withAnsi
      | otherwise = acc <> preAnsi

nonControlAnsi :: Text -> Bool
nonControlAnsi ansi = case T.unsnoc ansi of
  -- 'm' equals color: only code we consider 'good' for now
  Just (_, 'm') -> True
  _ -> False

-- tuple is: (text, ansi_code, ansi_code <> text)
-- example: splitAnsi "foo\ESC[0;3mbar"
splitAnsi :: Text -> [(Text, Text, Text)]
splitAnsi "" = []
splitAnsi t =
  -- (foo, \ESC[0;3m, bar) : ...
  (preAnsi, ansiCode, preAnsi <> ansiCode) : rest
  where
    -- (foo, \ESC[0;3mbar)
    (!preAnsi, !withAnsiFull) = T.breakOn "\ESC[" t
    -- (\ESC[0;3, mbar)
    (!ansiCodeNoChar, !withAnsiChar) = T.break isLetter withAnsiFull
    -- (\ESC[0;3m, bar)
    (!ansiCode, !rest) = case T.uncons withAnsiChar of
      -- (m, bar)
      Just (!ansiChar, !rest') -> (T.snoc ansiCodeNoChar ansiChar, splitAnsi rest')
      Nothing -> (ansiCodeNoChar, [])

-- | Parses bytes with arbitrary units and converts to bytes. First attempts
-- to parse as a 'Natural' so we do not lose precision. If that fails, falls
-- back to 'Double'.
--
-- ==== __Examples__
--
-- >>> parseByteText "120 mb"
-- Right (MkBytes 120000000)
--
-- >>> parseByteText "4.5 terabytes"
-- Right (MkBytes 4500000000000)
--
-- @since 0.5
parseByteText :: Text -> Either Text (Bytes B Natural)
parseByteText txt =
  case parse @(SomeSize Natural) txt of
    Right b -> Right $ convert (Proxy @B) b
    Left _ -> case parse @(SomeSize Double) txt of
      Right b -> Right (truncate <$> convert (Proxy @B) b)
      Left err -> Left err
