{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Utils
  ( -- * Timing Utils
    diffTime,
    formatTime,

    -- * Misc Utils
    headMaybe,

    -- * Functor Utils
    UtilsI.monoBimap,

    -- * Monad Utils
    whileNothing,

    -- * Math Utils
    UtilsI.divWithRem,
  )
where

import Data.Text (Text)
import ShellRun.Math (NonNegative)
import ShellRun.Math qualified as Math
import ShellRun.Utils.Internal qualified as UtilsI
import System.Clock (TimeSpec)
import System.Clock qualified as C

-- | For given \(x, y\), returns the absolute difference \(|x - y|\).
diffTime :: TimeSpec -> TimeSpec -> NonNegative
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-negative.
      Math.unsafeNonNegative diff

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
formatTime :: NonNegative -> Text
formatTime = UtilsI.formatTimeSummary . UtilsI.secondsToTimeSummary

whileNothing :: Monad m => m (Maybe b) -> m a -> m b
whileNothing mb ma = do
  maybeB <- mb
  case maybeB of
    Nothing -> ma *> whileNothing mb ma
    Just b -> pure b

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
