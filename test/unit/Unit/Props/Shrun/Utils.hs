-- | Property tests for Shrun.Utils.
module Unit.Props.Shrun.Utils
  ( props,
  )
where

import Shrun.Utils qualified as U
import Test.Tasty qualified as T
import Unit.Prelude
import Unit.Props.Generators qualified as PGens

-- | Entry point for Shrun.Utils property tests.
props :: TestTree
props = T.testGroup "Shrun.Utils" [diffTimeProps]

diffTimeProps :: TestTree
diffTimeProps =
  testPropertyNamed "diffTime" "diffTimeProps" $
    property $ do
      t1 <- forAll PGens.genTimeSpec
      t2 <- forAll PGens.genTimeSpec
      let result = U.diffTime t1 t2
      assert $ result >= 0
