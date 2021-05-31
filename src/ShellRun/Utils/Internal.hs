{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Utils.Internal
  ( divWithRem,
    monoBimap,
    TimeSummary (..),
    secondsToTimeSummary,
    formatTimeSummary,
  )
where

import Data.Bifunctor (Bifunctor)
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable qualified as Fold
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Math (NonNegative, Positive, REquals (..))
import ShellRun.Math qualified as Math

-- | For \(n \ge 0, d > 0\), returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r < n \\
--    \end{align}
-- \]
divWithRem :: NonNegative -> Positive -> (NonNegative, NonNegative)
divWithRem n d = monoBimap Math.unsafeNonNegative (n' `div` d', n' `rem` d')
  where
    n' = Math.getNonNegative n
    d' = Math.getPositive d

data TimeSummary = MkTimeSummary
  { days :: NonNegative,
    hours :: NonNegative,
    minutes :: NonNegative,
    seconds :: NonNegative
  }
  deriving (Show)

secondsToTimeSummary :: NonNegative -> TimeSummary
secondsToTimeSummary nn = MkTimeSummary days hours minutes seconds
  where
    (days, daysRem) = divWithRem nn $ Math.unsafePositive 86_400
    (hours, hoursRem) = divWithRem daysRem $ Math.unsafePositive 3_600
    (minutes, seconds) = divWithRem hoursRem $ Math.unsafePositive 60

formatTimeSummary :: TimeSummary -> Text
formatTimeSummary (MkTimeSummary d h m s) =
  let f acc (n, units)
        | n =:= (0 :: Int) = acc
        | otherwise = pluralize n units : acc
      vals = Fold.foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]
   in T.intercalate ", " vals

pluralize :: NonNegative -> Text -> Text
pluralize val txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    n = Math.getNonNegative val
    valUnit = T.pack (show n) <> txt

monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = Bifunctor.bimap f f