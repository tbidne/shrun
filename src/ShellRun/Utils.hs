{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities.
--
-- @since 0.1.0.0
module ShellRun.Utils
  ( -- * Text Utils
    breakStripPoint,
    decodeUtf8Lenient,
    splitOn,
    truncateIfNeeded,

    -- * Timing Utils
    diffTime,
    foldMap1,

    -- * Math Utils
    divWithRem,
  )
where

import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncErr
import GHC.Exts (IsList (..))
import GHC.Int (Int64)
import GHC.Stack (HasCallStack)
import Numeric.Algebra (NonZero (..))
import Refined (Refined)
import Refined qualified as R
import Refined.Extras (pattern MkRefined)
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Prelude
import System.Clock (TimeSpec (..))
import System.Clock qualified as C

-- $setup
-- >>> :set -XOverloadedLists
-- >>> :set -XTemplateHaskell
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Numeric.Algebra (mkAMonoidNonZeroTH)

-- | For \(n \ge 0, d > 0\), @divWithRem n d@ returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r \le n \\
--    \end{align}
-- \]
--
-- ==== __Examples__
-- >>> :{
--   let n = 34
--       d = $$(mkAMonoidNonZeroTH @Int 5)
--       result = divWithRem n d
--   in result
-- :}
-- (6,4)
--
-- >>> :{
--   let n = 12
--       d = $$(mkAMonoidNonZeroTH 18)
--       result = divWithRem n d
--   in result
-- :}
-- (0,12)
--
-- @since 0.1.0.0
divWithRem :: Integral a => a -> NonZero a -> Tuple2 a a
divWithRem n (MkNonZero d) = (n `div` d, n `rem` d)

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
-- @since 0.1.0.0
diffTime :: TimeSpec -> TimeSpec -> Natural
diffTime t1 t2 = i642n $ C.sec $ C.diffTimeSpec t1 t2
  where
    -- Allegedly safe because 'C.diffTimeSpec' guaranteed to be non-negative.
    i642n :: Int64 -> Natural
    i642n = fromIntegral

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
-- @since 0.1.0.0
foldMap1 :: (Foldable f, Semigroup s) => (a -> s) -> a -> f a -> s
foldMap1 f x xs = foldr (\b g y -> f y <> g b) f xs x

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
-- >>> -- ShellRun.Utils.Text
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
-- @since 0.1.0.0
breakStripPoint :: Refined R.NonEmpty Text -> Text -> Tuple2 Text Text
breakStripPoint (MkRefined point) txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair

-- | Decodes a 'ByteString' to UTF-8 in lenient mode.
--
-- @since 0.1.0.0
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncErr.lenientDecode

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
-- @since 0.1.0.0
splitOn :: HasCallStack => Refined R.NonEmpty Text -> Text -> NonEmptySeq Text
splitOn (MkRefined s) txt = case T.splitOn s txt of
  [] -> error err
  (t : ts) -> t :|^ fromList ts
  where
    err =
      "[ShellRun.Utils] Impossible: Text.splitOn returned empty list. Split: "
        <> s
        <> ", text: "
        <> txt

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
-- @since 0.1.0.0
truncateIfNeeded :: Natural -> Text -> Text
truncateIfNeeded n txt
  | T.length txt <= n' = txt
  | otherwise = txt'
  where
    txt' = T.take (n' - 3) txt <> "..."
    n' = n2i n

n2i :: Natural -> Int
n2i = fromIntegral
