-- | Property tests for ShellRun.Math.Positive.
module Props.ShellRun.Math.Positive (props) where

import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators qualified as PGens
import ShellRun.Math.Positive qualified as P
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Math.Positive property tests.
props :: TestTree
props = T.testGroup "ShellRun.Math.Positive" [mkPositiveProps, unsafePositive]

mkPositiveProps :: TestTree
mkPositiveProps = TH.testProperty "mkPositive" $
  H.property $ do
    n <- H.forAll PGens.genInt
    let result = P.mkPositive n
    H.assert $ case result of
      Just _ -> n > 0
      Nothing -> n <= 0

unsafePositive :: TestTree
unsafePositive = TH.testProperty "unsafePositive" $
  H.property $ do
    p <- H.forAll PGens.genPositive
    p === P.unsafePositive (P.getPositive p)
