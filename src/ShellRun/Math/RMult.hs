-- | Provides the 'RMult' typeclass for safe multiplication.
module ShellRun.Math.RMult
  ( RMult (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P

-- | The 'RMult' typeclass provides multiplication. The intention is to be used
-- with newtype'd numbers that carry some sort of invariant, e.g.,
-- 'NonNegative'. This way we can safely add numbers without losing their
-- invariants.
class RMult a b where
  type Prod a b
  (*:*) :: a -> b -> Prod a b

infixl 7 *:*

-- | 'NonNegative' * 'NonNegative' = 'NonNegative'
instance RMult NonNegative NonNegative where
  type Prod NonNegative NonNegative = NonNegative
  x *:* y = NN.unsafeNonNegative $ x' * y'
    where
      x' = NN.getNonNegative x
      y' = NN.getNonNegative y

-- | 'NonNegative' * 'Positive' = 'NonNegative'
instance RMult NonNegative Positive where
  type Prod NonNegative Positive = NonNegative
  x *:* y = NN.unsafeNonNegative $ x' * y'
    where
      x' = NN.getNonNegative x
      y' = P.getPositive y

-- | 'Positive' * 'NonNegative' = 'NonNegative'
instance RMult Positive NonNegative where
  type Prod Positive NonNegative = NonNegative
  x *:* y = NN.unsafeNonNegative $ x' * y'
    where
      x' = P.getPositive x
      y' = NN.getNonNegative y

-- | 'Positive' * 'Positive' = 'Positive'
instance RMult Positive Positive where
  type Prod Positive Positive = Positive
  x *:* y = P.unsafePositive $ x' * y'
    where
      x' = P.getPositive x
      y' = P.getPositive y
