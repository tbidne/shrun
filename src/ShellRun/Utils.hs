{-# LANGUAGE ImportQualifiedPost #-}

-- | Provides utilities.
module ShellRun.Utils
  ( -- * Text Utils
    module TextUtils,

    -- * Timing Utils
    diffTime,
    formatTime,

    -- * Misc Utils
    headMaybe,

    -- * Functor Utils
    UtilsI.monoBimap,

    -- * Math Utils
    UtilsI.divWithRem,
  )
where

import Data.Text (Text)
import ShellRun.Math (NonNegative)
import ShellRun.Math qualified as Math
import ShellRun.Utils.Internal qualified as UtilsI
import ShellRun.Utils.Text as TextUtils
import System.Clock (TimeSpec (..))
import System.Clock qualified as C

-- $ >>> :set -XNumericUnderscores

-- | For given \(x, y\), returns the absolute difference \(|x - y|\)
-- in seconds.
--
-- >>> :{
--   let t1 = TimeSpec 5 0
--       -- 20 s + 1 billion ns = 21 s
--       t2 = TimeSpec 20 1_000_000_000
--   in diffTime t1 t2
-- :}
-- MkNonNegative {getNonNegative = 16}
diffTime :: TimeSpec -> TimeSpec -> NonNegative
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-negative.
      Math.unsafeNonNegative diff

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
--
-- >>> :{
--   -- 2 days, 7 hours, 33 minutes, 20 seconds
--   let totalSeconds = Math.unsafeNonNegative 200_000
--   in formatTime totalSeconds
-- :}
-- "2 days, 7 hours, 33 minutes, 20 seconds"
formatTime :: NonNegative -> Text
formatTime = UtilsI.formatTimeSummary . UtilsI.secondsToTimeSummary

-- | Safe @head@.
--
-- >>> headMaybe [1,2,3]
-- Just 1
--
-- >>> headMaybe []
-- Nothing
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
