-- | Property tests for ShellRun.Utils.
module Unit.Props.ShellRun.Utils
  ( props,
  )
where

import Hedgehog qualified as H
import ShellRun.Utils qualified as U
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude
import Unit.Props.Generators qualified as PGens

-- | Entry point for ShellRun.Utils property tests.
props :: TestTree
props = T.testGroup "ShellRun.Utils" [diffTimeProps, divWithRemProps]

diffTimeProps :: TestTree
diffTimeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "diffTime" $
    H.withTests limit $
      H.property $ do
        t1 <- H.forAll PGens.genTimeSpec
        t2 <- H.forAll PGens.genTimeSpec
        let result = U.diffTime t1 t2
        H.assert $ result >= 0

divWithRemProps :: TestTree
divWithRemProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "divWithRem" $
    H.withTests limit $
      H.property $ do
        nn <- H.forAll PGens.genNonNegative
        pos <- H.forAll PGens.genPositive
        let result = U.divWithRem nn pos
        vDivWithRem (nn, pos) result

vDivWithRem :: Tuple2 Natural (NonZero Natural) -> Tuple2 Natural Natural -> PropertyT IO ()
vDivWithRem (n, MkNonZero divisor) (e, remainder) = do
  H.assert $ (divisor * e) + remainder == n
  H.footnote $
    "("
      <> show divisor
      <> " * "
      <> show e
      <> ") + "
      <> show remainder
      <> " == "
      <> show n
  H.assert $ remainder <= n
  H.footnote $ show remainder <> " <= " <> show n
