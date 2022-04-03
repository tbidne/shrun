{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'TimeRep' type and related functions for representing
-- time.
--
-- @since 0.1
module ShellRun.Data.TimeRep
  ( -- * Type
    TimeRep (..),

    -- * Conversions
    toSeconds,
    fromSeconds,

    -- * Formatting
    formatTimeRep,
    formatTime,

    -- * Constants

    -- ** Natural
    secondsInDay,
    secondsInHour,
    secondsInMinute,

    -- ** NonZero
    secondsInDayNZ,
    secondsInHourNZ,
    secondsInMinuteNZ,
  )
where

import Data.Text qualified as T
import Numeric.Algebra.Multiplicative.MGroup (MGroupIntegral (..))
import Numeric.Data.NonZero qualified as NZ
import ShellRun.Prelude

-- $setup
-- >>> :set -XTemplateHaskell

-- | Represents a relative time.
--
-- @since 0.1
data TimeRep = MkTimeRep
  { -- | @since 0.1
    days :: Natural,
    -- | @since 0.1
    hours :: Natural,
    -- | @since 0.1
    minutes :: Natural,
    -- | @since 0.1
    seconds :: Natural
  }
  deriving
    ( -- | @since 0.1
      Show
    )

-- | Transforms a 'TimeRep' into 'Natural' seconds.
--
-- ==== __Examples__
-- >>> :{
--        -- 200,000 seconds
--    let timeRep = MkTimeRep 2 7 33 20
--    in toSeconds timeRep
-- :}
-- 200000
--
-- @since 0.1
toSeconds :: TimeRep -> Natural
toSeconds (MkTimeRep d h m s) =
  d * secondsInDay
    + h * secondsInHour
    + m * secondsInMinute
    + s

-- | Transforms 'Natural' seconds into a 'TimeRep'.
--
-- ==== __Examples__
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds 200_000
--   in timeRep
-- :}
-- MkTimeRep {days = 2, hours = 7, minutes = 33, seconds = 20}
--
-- @since 0.1
fromSeconds :: Natural -> TimeRep
fromSeconds seconds = MkTimeRep d h m s
  where
    (d, daysRem) = seconds `gdivMod` secondsInDayNZ
    (h, hoursRem) = daysRem `gdivMod` secondsInHourNZ
    (m, s) = hoursRem `gdivMod` secondsInMinuteNZ

-- | Formats a 'TimeRep' to 'Text'.
--
-- ==== __Examples__
-- >>> :{
--   let -- 2 days, 7 hours, 33 minutes, 20 seconds
--       timeRep = fromSeconds 200_000
--   in formatTimeRep timeRep
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
--
-- @since 0.1
formatTimeRep :: TimeRep -> Text
formatTimeRep (isZero -> True) = "0 seconds"
formatTimeRep (MkTimeRep d h m s) = T.intercalate ", " vals
  where
    f acc (n, units)
      | n == 0 = acc
      | otherwise = pluralize n units : acc
    vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
--
-- ==== __Examples__
-- >>> :{
--   -- 2 days, 7 hours, 33 minutes, 20 seconds
--   let totalSeconds = 200_000
--   in formatTime totalSeconds
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
--
-- @since 0.1
formatTime :: Natural -> Text
formatTime = formatTimeRep . fromSeconds

isZero :: TimeRep -> Bool
isZero (MkTimeRep d h m s)
  | timeSum == 0 = True
  | otherwise = False
  where
    timeSum = foldl' sumUp 0 [d, h, m, s]
    sumUp acc = (+) acc

pluralize :: Natural -> Text -> Text
pluralize n txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    valUnit = showt n <> txt

-- | 'NonZero' seconds in a day: 86,400
--
-- >>> secondsInDayNZ
-- UnsafeNonZero 86400
--
-- @since 0.1
secondsInDayNZ :: NonZero Natural
secondsInDayNZ = $$(NZ.mkNonZeroTH 86_400)

-- | 'NonZero' seconds in an hour: 3,600
--
-- >>> secondsInHourNZ
-- UnsafeNonZero 3600
--
-- @since 0.1
secondsInHourNZ :: NonZero Natural
secondsInHourNZ = $$(NZ.mkNonZeroTH 3_600)

-- | 'NonZero' seconds in a minute: 60
--
-- >>> secondsInMinuteNZ
-- UnsafeNonZero 60
--
-- @since 0.1
secondsInMinuteNZ :: NonZero Natural
secondsInMinuteNZ = $$(NZ.mkNonZeroTH 60)

-- | Seconds in a day: 86,400
--
-- >>> secondsInDay
-- 86400
--
-- @since 0.1
secondsInDay :: Natural
secondsInDay = 86_400

-- | Seconds in an hour: 3,600
--
-- >>> secondsInHour
-- 3600
--
-- @since 0.1
secondsInHour :: Natural
secondsInHour = 3_600

-- | Seconds in a minute: 60
--
-- >>> secondsInMinute
-- 60
--
-- @since 0.1
secondsInMinute :: Natural
secondsInMinute = 60

makeFieldLabelsNoPrefix ''TimeRep
