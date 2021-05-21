{-# LANGUAGE ImportQualifiedPost #-}

module Props.Generators
  ( genNonNegative,
    genPositive,
    genTimeSpec,
    genInt,
  )
where

import Data.Int (Int64)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Types.NonNegative (NonNegative (..))
import ShellRun.Types.NonNegative qualified as NN
import ShellRun.Types.Positive (Positive (..))
import ShellRun.Types.Positive qualified as P
import System.Clock (TimeSpec (..))

genNonNegative :: Gen NonNegative
genNonNegative = NN.unsafeNonNegative <$> Gen.integral (Range.constant 0 1_000_000)

genPositive :: Gen Positive
genPositive = P.unsafePositive <$> Gen.integral (Range.constant 1 1_000_000)

genTimeSpec :: Gen TimeSpec
genTimeSpec = do
  TimeSpec
    <$> genInt64
    <*> genInt64

genInt64 :: Gen Int64
genInt64 =
  let range = Range.linearFrom 0 minBound maxBound
   in Gen.int64 range

genInt :: Gen Int
genInt =
  let range = Range.linearFrom 0 minBound maxBound
   in Gen.int range