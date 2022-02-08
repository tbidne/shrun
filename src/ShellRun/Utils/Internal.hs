{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Internal module for utilities.
module ShellRun.Utils.Internal
  ( divWithRem,
    TimeSummary (..),
    secondsToTimeSummary,
    formatTimeSummary,
  )
where

import Data.Text qualified as T
import Refined (NonNegative, Positive, Refined)
import Refined qualified as R
import Refined.Unsafe qualified as R
import ShellRun.Prelude

-- $setup
-- >>> :set -XTemplateHaskell

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
divWithRem ::
  Refined NonNegative Int ->
  Refined Positive Int ->
  (Refined NonNegative Int, Refined NonNegative Int)
divWithRem n d = monoBimap R.unsafeRefine (n' `div` d', n' `rem` d')
  where
    n' = R.unrefine n
    d' = R.unrefine d

-- | Represents a relative time.
data TimeSummary = MkTimeSummary
  { days :: Refined NonNegative Int,
    hours :: Refined NonNegative Int,
    minutes :: Refined NonNegative Int,
    seconds :: Refined NonNegative Int
  }
  deriving (Show)

-- | Transforms 'NonNegative' @seconds@ into a 'TimeSummary'.
--
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       summary = secondsToTimeSummary $$(R.refineTH @NonNegative @Int 200_000)
--       showSummary (MkTimeSummary d h m s) = fmap R.unrefine [d, h, m, s]
--   in showSummary summary
-- :}
-- [2,7,33,20]
secondsToTimeSummary :: Refined NonNegative Int -> TimeSummary
secondsToTimeSummary nn = MkTimeSummary d h m s
  where
    (d, daysRem) = divWithRem nn $$(R.refineTH 86_400)
    (h, hoursRem) = divWithRem daysRem $$(R.refineTH 3_600)
    (m, s) = divWithRem hoursRem $$(R.refineTH 60)

-- | Formats a 'TimeSummary' to 'Text'.
--
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       summary = secondsToTimeSummary $$(R.refineTH 200_000)
--   in formatTimeSummary summary
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
formatTimeSummary :: TimeSummary -> Text
formatTimeSummary (isZero -> True) = "0 seconds"
formatTimeSummary (MkTimeSummary d h m s) =
  let f :: [Text] -> (Refined NonNegative Int, Text) -> [Text]
      f acc (n, units)
        | n == $$(R.refineTH 0) = acc
        | otherwise = pluralize n units : acc
      vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]
   in T.intercalate ", " vals

isZero :: TimeSummary -> Bool
isZero (MkTimeSummary d h m s)
  | timeSum == 0 = True
  | otherwise = False
  where
    timeSum = foldl' sumUp 0 [d, h, m, s]
    sumUp acc = (+) acc . R.unrefine

pluralize :: Refined NonNegative Int -> Text -> Text
pluralize val txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    n = R.unrefine val
    valUnit = showt n <> txt
