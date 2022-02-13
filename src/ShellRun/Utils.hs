{-# LANGUAGE ViewPatterns #-}

-- | Provides utilities.
module ShellRun.Utils
  ( -- * Text Utils
    breakStripPoint,
    decodeUtf8Lenient,

    -- * Timing Utils
    diffTime,

    -- * Misc Utils
    displayCommand,

    -- * Math Utils
    divWithRem,
  )
where

import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncErr
import Refined (Refined)
import Refined qualified as R
import Refined.Unsafe qualified as R
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env (CommandDisplay (..))
import ShellRun.Prelude
import System.Clock (TimeSpec (..))
import System.Clock qualified as C

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Refined (NonNegative, Positive)

-- | For \(n \ge 0, d > 0\), @divWithRem n d@ returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r \le n \\
--    \end{align}
-- \]
--
-- Examples:
--
-- >>> :{
--   let n = $$(R.refineTH @NonNegative @Int 34)
--       d = $$(R.refineTH @Positive @Int 5)
--       result = divWithRem n d
--   in monoBimap R.unrefine result
-- :}
-- (6,4)
--
-- >>> :{
--   let n = $$(R.refineTH @NonNegative @Int 12)
--       d = $$(R.refineTH @Positive @Int 18)
--       result = divWithRem n d
--   in monoBimap R.unrefine result
-- :}
-- (0,12)
divWithRem :: RNonNegative -> RPositive -> Tuple2 RNonNegative RNonNegative
divWithRem n d = monoBimap R.unsafeRefine (n' `div` d', n' `rem` d')
  where
    n' = R.unrefine n
    d' = R.unrefine d

-- | For given \(x, y\), returns the absolute difference \(|x - y|\)
-- in seconds.
--
-- >>> :{
--   let t1 = TimeSpec 5 0
--       -- 20 s + 1 billion ns = 21 s
--       t2 = TimeSpec 20 1_000_000_000
--   in diffTime t1 t2
-- :}
-- Refined 16
diffTime :: TimeSpec -> TimeSpec -> RNonNegative
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-negative.
      R.unsafeRefine diff

-- | Returns the key if one exists and we pass in 'ShowKey', otherwise
-- returns the command.
--
-- >>> displayCommand ShowKey (MkCommand Nothing "cmd")
-- "cmd"
--
-- >>> displayCommand ShowCommand (MkCommand (Just "key") "cmd")
-- "cmd"
--
-- >>> displayCommand ShowKey (MkCommand (Just "key") "cmd")
-- "key"
displayCommand :: CommandDisplay -> Command -> Text
displayCommand ShowKey (MkCommand (Just key) _) = key
displayCommand _ (MkCommand _ cmd) = cmd

-- | Wrapper for 'Text'\'s 'T.breakOn' that differs in two ways:
--
-- 1. Total, since we restrict the @needle@ to 'R.NonEmpty'
-- ('Text' throws a pure exception here).
-- 2. If the @needle@ is found within the @haystack@, we do not include it
-- in the second part of the pair.
--
-- For instance:
--
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
breakStripPoint :: Refined R.NonEmpty Text -> Text -> Tuple2 Text Text
breakStripPoint rpoint txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair
  where
    point = R.unrefine rpoint

-- | Decodes a 'Bytestring' to UTF-8 in lenient mode.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncErr.lenientDecode
