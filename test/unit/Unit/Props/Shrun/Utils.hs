-- | Property tests for Shrun.Utils.
module Unit.Props.Shrun.Utils
  ( props,
  )
where

import Hedgehog qualified as H
import Shrun.Utils qualified as U
import Test.Tasty qualified as T
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude
import Unit.Props.Generators qualified as PGens

-- | Entry point for Shrun.Utils property tests.
props :: TestTree
props = T.testGroup "Shrun.Utils" [diffTimeProps]

diffTimeProps :: TestTree
diffTimeProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "diffTime" "diffTimeProps" $
    H.withTests limit $
      H.property $ do
        t1 <- H.forAll PGens.genTimeSpec
        t2 <- H.forAll PGens.genTimeSpec
        let result = U.diffTime t1 t2
        H.assert $ result >= 0
