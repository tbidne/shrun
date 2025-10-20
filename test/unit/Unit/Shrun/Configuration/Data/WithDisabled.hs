module Unit.Shrun.Configuration.Data.WithDisabled
  ( tests,
  )
where

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled
      ( Disabled,
        With
      ),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Data.WithDisabled"
    [ testsFunctions
    ]

testsFunctions :: TestTree
testsFunctions =
  testGroup
    "Functions"
    [ testsToMaybe
    ]

testsToMaybe :: TestTree
testsToMaybe = testPropertyNamed desc name $ do
  property $ do
    x <- forAll genInt

    Just x === WD.toMaybe (With x)
    Nothing === WD.toMaybe (Disabled @Int)
  where
    desc = "testsToMaybe"
    name = "toMaybe"

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100
