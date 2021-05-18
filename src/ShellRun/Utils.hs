module ShellRun.Utils
  ( -- * Timing Utils
    diffTime,
    formatSeconds,

    -- * Functor Utils
    monoBimap,
  )
where

import Data.Bifunctor (Bifunctor)
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Types.NonNegative qualified as NN
import ShellRun.Types.Positive (Positive)
import ShellRun.Types.Positive qualified as P
import System.Clock qualified as C

-- | For given \(x, y\), returns the absolute difference \(|x - y|\).
diffTime :: C.TimeSpec -> C.TimeSpec -> NonNegative
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

-- | For \(n \ge 0\) seconds, returns a 'T.Text' description of the minutes
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
          pluralize s " second",
          "  "
        ]

monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = Bifunctor.bimap f f