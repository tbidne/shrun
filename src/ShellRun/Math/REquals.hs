-- | Provides the 'REquals' class.
module ShellRun.Math.REquals
  ( REquals (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P
import ShellRun.Prelude

-- | 'REquals' is used for convenience when we would like to compare
-- different types for equality. This makes sense when both types
-- are wrappers over the same type. Because 'Eq' should be favored
-- whenever possible, no instances for @REquals a a@ are provided.
class REquals a b where
  (=:=) :: a -> b -> Bool

instance REquals NonNegative Int where
  (=:=) :: NonNegative -> Int -> Bool
  nn =:= x = nn' == x
    where
      nn' = NN.getNonNegative nn

instance REquals Int NonNegative where
  (=:=) :: Int -> NonNegative -> Bool
  x =:= nn = nn' == x
    where
      nn' = NN.getNonNegative nn

instance REquals Positive Int where
  (=:=) :: Positive -> Int -> Bool
  nn =:= x = nn' == x
    where
      nn' = P.getPositive nn

instance REquals Int Positive where
  (=:=) :: Int -> Positive -> Bool
  x =:= nn = nn' == x
    where
      nn' = P.getPositive nn

instance REquals NonNegative Positive where
  (=:=) :: NonNegative -> Positive -> Bool
  nn =:= p = nn' == p'
    where
      nn' = NN.getNonNegative nn
      p' = P.getPositive p

instance REquals Positive NonNegative where
  (=:=) :: Positive -> NonNegative -> Bool
  p =:= nn = nn' == p'
    where
      nn' = NN.getNonNegative nn
      p' = P.getPositive p
