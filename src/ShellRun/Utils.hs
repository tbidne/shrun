{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Utils
  ( -- * Timing Utils
    diffTime,
    formatSeconds,

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

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the minutes
-- and seconds.
formatSeconds :: NonNegative -> Text
formatSeconds seconds =
  let d = P.unsafePositive 60
      (m, s) = divWithRem seconds d
      pluralize i t
        | NN.getNonNegative i == 1 = t
        | otherwise = t <> "s"
   in T.concat
        [ NN.prettyPrint m,
          pluralize m " minute",
          " and ",
          NN.prettyPrint s,
          pluralize s " second"
        ]

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
