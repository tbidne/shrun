{-# LANGUAGE TemplateHaskell #-}

-- | Exports common TH constants.
--
-- @since 0.1.0.0
module ShellRun.Data.TH
  ( -- * Numeric

    -- ** NonNegative
    zeroNN,
    oneNN,
    dayNN,
    hourNN,
    minuteNN,

    -- ** Positive
    dayPos,
    hourPos,
    minutePos,

    -- * Text
    equalsNE,
  )
where

import Refined (Refined)
import Refined qualified as R
import ShellRun.Prelude

-- | 0.
--
-- @since 0.1.0.0
zeroNN :: RNonNegative
zeroNN = $$(R.refineTH 0)

-- | 1.
--
-- @since 0.1.0.0
oneNN :: RNonNegative
oneNN = $$(R.refineTH 1)

-- | Seconds in a day: 86,400.
--
-- @since 0.1.0.0
dayNN :: RNonNegative
dayNN = $$(R.refineTH 86_400)

-- | Seconds in an hour: 3,600.
--
-- @since 0.1.0.0
hourNN :: RNonNegative
hourNN = $$(R.refineTH 3_600)

-- | Seconds in a minute: 60.
--
-- @since 0.1.0.0
minuteNN :: RNonNegative
minuteNN = $$(R.refineTH 60)

-- | Seconds in a day: 86,400.
--
-- @since 0.1.0.0
dayPos :: RPositive
dayPos = $$(R.refineTH 86_400)

-- | Seconds in an hour: 3,600.
--
-- @since 0.1.0.0
hourPos :: RPositive
hourPos = $$(R.refineTH 3_600)

-- | Seconds in a minute: 60.
--
-- @since 0.1.0.0
minutePos :: RPositive
minutePos = $$(R.refineTH 60)

-- | Breakpoint for our legend key.
--
-- @since 0.1.0.0
equalsNE :: Refined R.NonEmpty Text
equalsNE = $$(R.refineTH "=")
