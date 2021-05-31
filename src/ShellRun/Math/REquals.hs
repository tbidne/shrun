{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Math.REquals
  ( REquals (..),
  )
where

import ShellRun.Math.NonNegative (NonNegative)
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Math.Positive (Positive)
import ShellRun.Math.Positive qualified as P

class REquals a b where
  (=:=) :: a -> b -> Bool

instance Integral a => REquals NonNegative a where
  nn =:= x = nn' == x'
    where
      x' = fromIntegral x
      nn' = NN.getNonNegative nn

instance Integral a => REquals a NonNegative where
  x =:= nn = nn' == x'
    where
      x' = fromIntegral x
      nn' = NN.getNonNegative nn

instance Integral a => REquals Positive a where
  nn =:= x = nn' == x'
    where
      x' = fromIntegral x
      nn' = P.getPositive nn

instance Integral a => REquals a Positive where
  x =:= nn = nn' == x'
    where
      x' = fromIntegral x
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