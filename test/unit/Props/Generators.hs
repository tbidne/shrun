{-# LANGUAGE ImportQualifiedPost #-}

module Props.Generators
  ( genNonNegative,
    genPositive,
    genTimeSpec,
    genInt,
    genText,
    getNonEmptyText,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Math (NonNegative (..), Positive (..))
import ShellRun.Math qualified as Math
import ShellRun.Utils.Text (NonEmptyText)
import ShellRun.Utils.Text qualified as TextUtils
import System.Clock (TimeSpec (..))

genNonNegative :: Gen NonNegative
genNonNegative = Math.unsafeNonNegative <$> Gen.integral (Range.constant 0 1_000_000)

genPositive :: Gen Positive
genPositive = Math.unsafePositive <$> Gen.integral (Range.constant 1 1_000_000)

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

genText :: Gen Text
genText = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 0 0 30

getNonEmptyText :: Gen NonEmptyText
getNonEmptyText = TextUtils.unsafeMkNonEmptyText <$> Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 30
