module Props.ShellRun.Utils
  ( props,
  )
where

import Hedgehog (Gen, PropertyT)
import Hedgehog qualified as H
import Props.Generators qualified as PGens
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Types.NonNegative qualified as NN
import ShellRun.Types.Positive (Positive)
import ShellRun.Types.Positive qualified as P
import ShellRun.Utils qualified as U
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import System.Clock (TimeSpec)

props :: TestTree
props = T.testGroup "ShellRun.Utils" [diffTimeProps, divWithRemProps]

diffTimeProps :: TestTree
diffTimeProps = TH.testProperty "diffTime" $
  H.property $ do
    (t1, t2) <- H.forAll genTimeSpecs
    let result = U.diffTime t1 t2
    H.assert $ result >= NN.unsafeNonNegative 0

divWithRemProps :: TestTree
divWithRemProps = TH.testProperty "divWithRem" $
  H.property $ do
    input@(nn, pos) <- H.forAll genNNAndPos
    let result = U.divWithRem nn pos
    vDivWithRem input result

vDivWithRem :: (NonNegative, Positive) -> (NonNegative, NonNegative) -> PropertyT IO ()
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
    nRaw = NN.getNonNegative n
    divisorRaw = P.getPositive divisor
    eRaw = NN.getNonNegative e
    remainderRaw = NN.getNonNegative remainder

genTimeSpecs :: Gen (TimeSpec, TimeSpec)
genTimeSpecs = (,) <$> PGens.genTimeSpec <*> PGens.genTimeSpec 

genNNAndPos :: Gen (NonNegative, Positive)
genNNAndPos = (,) <$> PGens.genNonNegative <*> PGens.genPositive