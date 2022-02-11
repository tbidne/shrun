{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'TimeRep' type and related functions for representing
-- time.
module ShellRun.Data.TimeRep
  ( -- * Type
    TimeRep (..),

    -- * Conversions
    toSeconds,
    fromSeconds,

    -- * Formatting
    formatTimeRep,
    formatTime,
  )
where

import Data.Text qualified as T
import Numeric.Algebra (ASemigroup (..), MSemigroup (..))
import Refined (NonNegative, Positive, Refined)
import Refined qualified as R
import ShellRun.Prelude
import ShellRun.Utils qualified as U

-- $setup
-- >>> :set -XTemplateHaskell

-- | Represents a relative time.
data TimeRep = MkTimeRep
  { days :: Refined NonNegative Int,
    hours :: Refined NonNegative Int,
    minutes :: Refined NonNegative Int,
    seconds :: Refined NonNegative Int
  }
  deriving (Show)

-- | Transforms a 'TimeRep' into 'NonNegative' seconds.
--
-- >>> :{
--        -- 200,000 seconds
--    let timeRep =
--         MkTimeRep
--           $$(R.refineTH 2)
--           $$(R.refineTH 7)
--           $$(R.refineTH 33)
--           $$(R.refineTH 20)
--    in toSeconds timeRep
-- :}
-- Refined 200000
toSeconds :: TimeRep -> Refined NonNegative Int
toSeconds (MkTimeRep d h m s) =
  (d .*. dayNN)
    .+. (h .*. hourNN)
    .+. (m .*. minuteNN)
    .+. s

-- | Transforms 'NonNegative' @seconds@ into a 'TimeRep'.
--
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds $$(R.refineTH @NonNegative @Int 200_000)
--       showRep (MkTimeRep d h m s) = fmap R.unrefine [d, h, m, s]
--   in showRep timeRep
-- :}
-- [2,7,33,20]
fromSeconds :: Refined NonNegative Int -> TimeRep
fromSeconds seconds = MkTimeRep d h m s
  where
    (d, daysRem) = U.divWithRem seconds dayPos
    (h, hoursRem) = U.divWithRem daysRem hourPos
    (m, s) = U.divWithRem hoursRem minutePos

-- | Formats a 'TimeRep' to 'Text'.
--
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds $$(R.refineTH @NonNegative 200_000)
--   in formatTimeRep timeRep
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
formatTimeRep :: TimeRep -> Text
formatTimeRep (isZero -> True) = "0 seconds"
formatTimeRep (MkTimeRep d h m s) = T.intercalate ", " vals
  where
    f acc (n, units)
      | n == $$(R.refineTH @NonNegative @Int 0) = acc
      | otherwise = pluralize n units : acc
    vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
--
-- >>> :{
--   -- 2 days, 7 hours, 33 minutes, 20 seconds
--   let totalSeconds = $$(R.refineTH @NonNegative @Int 200_000)
--   in formatTime totalSeconds
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
formatTime :: Refined NonNegative Int -> Text
formatTime = formatTimeRep . fromSeconds

isZero :: TimeRep -> Bool
isZero (MkTimeRep d h m s)
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

zero :: Refined NonNegative Int
zero = $$(R.refineTH 0)

dayNN :: Refined NonNegative Int
dayNN = $$(R.refineTH 86_400)

hourNN :: Refined NonNegative Int
hourNN = $$(R.refineTH 3_600)

minuteNN :: Refined NonNegative Int
minuteNN = $$(R.refineTH 60)

dayPos :: Refined Positive Int
dayPos = $$(R.refineTH 86_400)

hourPos :: Refined Positive Int
hourPos = $$(R.refineTH 3_600)

minutePos :: Refined Positive Int
minutePos = $$(R.refineTH 60)
