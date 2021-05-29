{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Utils
  ( -- * Timing Utils
    diffTime,
    formatTime,

    -- * Misc Utils
    headMaybe,

    -- * Functor Utils
    monoBimap,

    -- * Monad Utils
    whileNothing,

    -- * Math Utils
    divWithRem,
  )
where

import Data.Bifunctor (Bifunctor)
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P
import System.Clock (TimeSpec)
import System.Clock qualified as C

-- | For given \(x, y\), returns the absolute difference \(|x - y|\).
diffTime :: TimeSpec -> TimeSpec -> NonNegative
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-negative.
      NN.unsafeNonNegative diff

-- | For \(n \ge 0, d > 0\), returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r < n \\
--    \end{align}
-- \]
divWithRem :: NonNegative -> Positive -> (NonNegative, NonNegative)
divWithRem n d = monoBimap NN.unsafeNonNegative (n' `div` d', n' `rem` d')
  where
    n' = NN.getNonNegative n
    d' = P.getPositive d

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
formatTime :: NonNegative -> Text
formatTime seconds
  | n < 60 = formatSeconds seconds
  | n < 3600 = formatMinutes seconds
  | n < 86400 = formatHours seconds
  | otherwise = formatDays seconds
  where
    n = NN.getNonNegative seconds

formatSeconds :: NonNegative -> Text
formatSeconds seconds = pluralize seconds " second"

formatMinutes :: NonNegative -> Text
formatMinutes seconds =
  let d = P.unsafePositive 60
      (m, s) = divWithRem seconds d
      minutesTxt = pluralize m " minute"
      secondsTxt = pluralize s " second"
   in minutesTxt <> " and " <> secondsTxt

formatHours :: NonNegative -> Text
formatHours seconds =
  let hoursDiv = P.unsafePositive 3600
      minutesDiv = P.unsafePositive 60
      (h, rest) = divWithRem seconds hoursDiv
      (m, s) = divWithRem rest minutesDiv
      hoursTxt = pluralize h " hour"
      minutesTxt = pluralize m " minute"
      secondsTxt = pluralize s " second"
   in hoursTxt <> ", " <> minutesTxt <> " and " <> secondsTxt

formatDays :: NonNegative -> Text
formatDays seconds =
  let daysDiv = P.unsafePositive 86400
      hoursDiv = P.unsafePositive 3600
      minutesDiv = P.unsafePositive 60
      (d, rest) = divWithRem seconds daysDiv
      (h, rest') = divWithRem rest hoursDiv
      (m, s) = divWithRem rest' minutesDiv
      daysTxt = pluralize d " day"
      hoursTxt = pluralize h " hour"
      minutesTxt = pluralize m " minute"
      secondsTxt = pluralize s " second"
   in daysTxt
        <> ", "
        <> hoursTxt
        <> ", "
        <> minutesTxt
        <> " and "
        <> secondsTxt

pluralize :: NonNegative -> Text -> Text
pluralize val txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    n = NN.getNonNegative val
    valUnit = T.pack (show n) <> txt

monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = Bifunctor.bimap f f

whileNothing :: Monad m => m (Maybe b) -> m a -> m b
whileNothing mb ma = do
  maybeB <- mb
  case maybeB of
    Nothing -> ma *> whileNothing mb ma
    Just b -> pure b

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
