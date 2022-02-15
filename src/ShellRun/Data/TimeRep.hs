{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'TimeRep' type and related functions for representing
-- time.
--
-- @since 0.1.0.0
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
import Refined qualified as R
import ShellRun.Data.TH qualified as TH
import ShellRun.Prelude
import ShellRun.Utils qualified as U

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Refined (NonNegative)

-- | Represents a relative time.
--
-- @since 0.1.0.0
data TimeRep = MkTimeRep
  { -- | @since 0.1.0.0
    days :: RNonNegative,
    -- | @since 0.1.0.0
    hours :: RNonNegative,
    -- | @since 0.1.0.0
    minutes :: RNonNegative,
    -- | @since 0.1.0.0
    seconds :: RNonNegative
  }
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | Transforms a 'TimeRep' into 'Refined.NonNegative' seconds.
--
-- ==== __Examples__
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
--
-- @since 0.1.0.0
toSeconds :: TimeRep -> RNonNegative
toSeconds (MkTimeRep d h m s) =
  (d .*. TH.dayNN)
    .+. (h .*. TH.hourNN)
    .+. (m .*. TH.minuteNN)
    .+. s

-- | Transforms 'Refined.NonNegative' seconds into a 'TimeRep'.
--
-- ==== __Examples__
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds $$(R.refineTH @NonNegative @Int 200_000)
--       showRep (MkTimeRep d h m s) = fmap R.unrefine [d, h, m, s]
--   in showRep timeRep
-- :}
-- [2,7,33,20]
--
-- @since 0.1.0.0
fromSeconds :: RNonNegative -> TimeRep
fromSeconds seconds = MkTimeRep d h m s
  where
    (d, daysRem) = U.divWithRem seconds TH.dayPos
    (h, hoursRem) = U.divWithRem daysRem TH.hourPos
    (m, s) = U.divWithRem hoursRem TH.minutePos

-- | Formats a 'TimeRep' to 'Text'.
--
-- ==== __Examples__
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds $$(R.refineTH @NonNegative 200_000)
--   in formatTimeRep timeRep
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
--
-- @since 0.1.0.0
formatTimeRep :: TimeRep -> Text
formatTimeRep (isZero -> True) = "0 seconds"
formatTimeRep (MkTimeRep d h m s) = T.intercalate ", " vals
  where
    f acc (n, units)
      | n == TH.zeroNN = acc
      | otherwise = pluralize n units : acc
    vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
--
-- ==== __Examples__
-- >>> :{
--   -- 2 days, 7 hours, 33 minutes, 20 seconds
--   let totalSeconds = $$(R.refineTH @NonNegative @Int 200_000)
--   in formatTime totalSeconds
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
--
-- @since 0.1.0.0
formatTime :: RNonNegative -> Text
formatTime = formatTimeRep . fromSeconds

isZero :: TimeRep -> Bool
isZero (MkTimeRep d h m s)
  | timeSum == 0 = True
  | otherwise = False
  where
    timeSum = foldl' sumUp 0 [d, h, m, s]
    sumUp acc = (+) acc . R.unrefine

pluralize :: RNonNegative -> Text -> Text
pluralize val txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    n = R.unrefine val
    valUnit = showt n <> txt
