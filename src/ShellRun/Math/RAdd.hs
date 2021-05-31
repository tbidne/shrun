{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module ShellRun.Math.RAdd
  ( RAdd (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P

class RAdd a b where
  type Sum a b
  (+:+) :: a -> b -> Sum a b

infixl 6 +:+

instance RAdd NonNegative NonNegative where
  type Sum NonNegative NonNegative = NonNegative
  x +:+ y = NN.unsafeNonNegative $ x' + y'
    where
      x' = NN.getNonNegative x
      y' = NN.getNonNegative y

instance RAdd NonNegative Positive where
  type Sum NonNegative Positive = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = NN.getNonNegative x
      y' = P.getPositive y

instance RAdd Positive NonNegative where
  type Sum Positive NonNegative = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = P.getPositive x
      y' = NN.getNonNegative y

instance RAdd Positive Positive where
  type Sum Positive Positive = Positive
  x +:+ y = P.unsafePositive $ x' + y'
    where
      x' = P.getPositive x
      y' = P.getPositive y
