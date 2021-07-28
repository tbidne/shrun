-- | Property tests for ShellRun.Math.NonNegative.
module Props.ShellRun.Math.NonNegative (props) where

import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators qualified as PGens
import ShellRun.Math.NonNegative qualified as NN
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Math.NonNegative property tests.
props :: TestTree
props = T.testGroup "ShellRun.Math.NonNegative" [mkNonNegativeProps, unsafeNonNegative]

mkNonNegativeProps :: TestTree
mkNonNegativeProps = TH.testProperty "mkNonNegative" $
  H.property $ do
    n <- H.forAll PGens.genInt
    let result = NN.mkNonNegative n
    H.assert $ case result of
      Just _ -> n >= 0
      Nothing -> n < 0

unsafeNonNegative :: TestTree
unsafeNonNegative = TH.testProperty "unsafeNonNegative" $
  H.property $ do
    n <- H.forAll PGens.genNonNegative
    n === NN.unsafeNonNegative (NN.getNonNegative n)
