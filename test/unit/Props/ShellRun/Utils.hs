{-# LANGUAGE TemplateHaskell #-}

-- | Property tests for ShellRun.Utils.
module Props.ShellRun.Utils
  ( props,
  )
where

import Hedgehog (Gen, PropertyT)
import Hedgehog qualified as H
import Props.Generators qualified as PGens
import Refined (NonNegative, Positive, Refined)
import Refined qualified as R
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Clock (TimeSpec)
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Utils property tests.
props :: TestTree
props = T.testGroup "ShellRun.Utils" [diffTimeProps, divWithRemProps]

diffTimeProps :: TestTree
diffTimeProps = TH.testProperty "diffTime" $
  H.property $ do
    t1 <- H.forAll PGens.genTimeSpec
    t2 <- H.forAll PGens.genTimeSpec
    let result = U.diffTime t1 t2
    H.assert $ result >= $$(R.refineTH 0)

divWithRemProps :: TestTree
divWithRemProps = TH.testProperty "divWithRem" $
  H.property $ do
    nn <- H.forAll PGens.genNonNegative
    pos <- H.forAll PGens.genPositive
    let result = U.divWithRem nn pos
    vDivWithRem (nn, pos) result

vDivWithRem :: (Refined NonNegative Int, Refined Positive Int) -> (Refined NonNegative Int, Refined NonNegative Int) -> PropertyT IO ()
vDivWithRem (n, divisor) (e, remainder) = do
  H.assert $ (divisorRaw * eRaw) + remainderRaw == nRaw
  H.footnote $
    "("
      <> show divisorRaw
      <> " * "
      <> show eRaw
      <> ") + "
      <> show remainderRaw
      <> " == "
      <> show nRaw
  H.assert $ remainderRaw <= nRaw
  H.footnote $ show remainderRaw <> " <= " <> show nRaw
  where
    nRaw = R.unrefine n
    divisorRaw = R.unrefine divisor
    eRaw = R.unrefine e
    remainderRaw = R.unrefine remainder

genTimeSpecs :: Gen (TimeSpec, TimeSpec)
genTimeSpecs = (,) <$> PGens.genTimeSpec <*> PGens.genTimeSpec
