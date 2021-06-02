{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides the 'RAdd' typeclass for safe addition.
module ShellRun.Math.RAdd
  ( RAdd (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P

-- | The 'RAdd' typeclass provides addition. The intention is to be used
-- with newtype'd numbers that carry some sort of invariant, e.g.,
-- 'NonNegative'. This way we can safely add numbers without losing their
-- invariants.
class RAdd a b where
  type Sum a b
  (+:+) :: a -> b -> Sum a b

infixl 6 +:+

-- | 'NonNegative' + 'NonNegative' = 'NonNegative'
instance RAdd NonNegative NonNegative where
  type Sum NonNegative NonNegative = NonNegative
  x +:+ y = NN.unsafeNonNegative $ x' + y'
    where
      x' = NN.getNonNegative x
      y' = NN.getNonNegative y

-- | 'NonNegative' + 'Positive' = 'Positive'
instance RAdd NonNegative Positive where
  type Sum NonNegative Positive = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = NN.getNonNegative x
      y' = P.getPositive y

-- | 'Positive' + 'NonNegative' = 'Positive'
instance RAdd Positive NonNegative where
  type Sum Positive NonNegative = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = P.getPositive x
      y' = NN.getNonNegative y

-- | 'Positive' + 'Positive' = 'Positive'
instance RAdd Positive Positive where
  type Sum Positive Positive = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = P.getPositive x
      y' = P.getPositive y
