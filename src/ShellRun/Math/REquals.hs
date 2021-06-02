{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides the 'REquals' class.
module ShellRun.Math.REquals
  ( REquals (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P

-- | 'REquals' is used for convenience when we would like to compare
-- different types for equality. This makes sense when both types
-- are wrappers over the same type. Because 'Eq' should be favored
-- whenever possible, no instances for @REquals a a@ are provided.
class REquals a b where
  (=:=) :: a -> b -> Bool

instance REquals NonNegative Int where
  nn =:= x = nn' == x
    where
      nn' = NN.getNonNegative nn

instance REquals Int NonNegative where
  x =:= nn = nn' == x
    where
      nn' = NN.getNonNegative nn

instance REquals Positive Int where
  nn =:= x = nn' == x
    where
      nn' = P.getPositive nn

instance REquals Int Positive where
  x =:= nn = nn' == x
    where
      nn' = P.getPositive nn

instance REquals NonNegative Positive where
  nn =:= p = nn' == p'
    where
      nn' = NN.getNonNegative nn
      p' = P.getPositive p

instance REquals Positive NonNegative where
  p =:= nn = nn' == p'
    where
      nn' = NN.getNonNegative nn
      p' = P.getPositive p