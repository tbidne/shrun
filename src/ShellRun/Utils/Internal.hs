{-# LANGUAGE ViewPatterns #-}

-- | Internal module for utilities.
module ShellRun.Utils.Internal
  ( divWithRem,
    TimeSummary (..),
    secondsToTimeSummary,
    formatTimeSummary,
  )
where

import Data.Foldable qualified as Fold
import Data.Text qualified as T
import ShellRun.Math (NonNegative, Positive, REquals (..))
import ShellRun.Math qualified as Math
import ShellRun.Prelude

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
--   let n = Math.unsafeNonNegative 34
--       d = Math.unsafePositive 5
--       result = divWithRem n d
--   in monoBimap Math.getNonNegative result
-- :}
-- (6,4)
--
-- >>> :{
--   let n = Math.unsafeNonNegative 12
--       d = Math.unsafePositive 18
--       result = divWithRem n d
--   in monoBimap Math.getNonNegative result
-- :}
-- (0,12)
divWithRem :: NonNegative -> Positive -> (NonNegative, NonNegative)
divWithRem n d = monoBimap Math.unsafeNonNegative (n' `div` d', n' `rem` d')
  where
    n' = Math.getNonNegative n
    d' = Math.getPositive d

-- | Represents a relative time.
data TimeSummary = MkTimeSummary
  { days :: NonNegative,
    hours :: NonNegative,
    minutes :: NonNegative,
    seconds :: NonNegative
  }
  deriving (Show)

-- | Transforms 'NonNegative' @seconds@ into a 'TimeSummary'.
--
-- >>> :{
--   let totalSeconds = 200_000 -- 2 days, 7 hours, 33 minutes, 20 seconds
--       summary = secondsToTimeSummary (Math.unsafeNonNegative totalSeconds)
--       showSummary (MkTimeSummary d h m s) = fmap Math.getNonNegative [d, h, m, s]
--   in showSummary summary
-- :}
-- [2,7,33,20]
secondsToTimeSummary :: NonNegative -> TimeSummary
secondsToTimeSummary nn = MkTimeSummary d h m s
  where
    (d, daysRem) = divWithRem nn $ Math.unsafePositive 86_400
    (h, hoursRem) = divWithRem daysRem $ Math.unsafePositive 3_600
    (m, s) = divWithRem hoursRem $ Math.unsafePositive 60

-- | Formats a 'TimeSummary' to 'Text'.
--
-- >>> :{
--   let totalSeconds = 200_000 -- 2 days, 7 hours, 33 minutes, 20 seconds
--       summary = secondsToTimeSummary (Math.unsafeNonNegative totalSeconds)
--   in formatTimeSummary summary
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
formatTimeSummary :: TimeSummary -> Text
formatTimeSummary (isZero -> True) = "0 seconds"
formatTimeSummary (MkTimeSummary d h m s) =
  let f acc (n, units)
        | n =:= (0 :: Int) = acc
        | otherwise = pluralize n units : acc
      vals = Fold.foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]
   in T.intercalate ", " vals

isZero :: TimeSummary -> Bool
isZero (MkTimeSummary d h m s)
  | timeSum == 0 = True
  | otherwise = False
  where
    timeSum = Fold.foldl sumUp 0 [d, h, m, s]
    sumUp acc = (+) acc . Math.getNonNegative

pluralize :: NonNegative -> Text -> Text
pluralize val txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    n = Math.getNonNegative val
    valUnit = showt n <> txt
