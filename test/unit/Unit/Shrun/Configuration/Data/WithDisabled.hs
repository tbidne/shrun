{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

module Unit.Shrun.Configuration.Data.WithDisabled
  ( tests,
  )
where

import Data.Monoid (Endo (Endo, appEndo))
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled
      ( Disabled,
        With,
        Without
      ),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Data.WithDisabled"
    [ testLaws,
      testsFunctions
    ]

testLaws :: TestTree
testLaws =
  testGroup
    "Laws"
    [ testAssociativity,
      testIdentity,
      testLeftBias,
      testDisableNormalSubmonoid,
      testFoldable
    ]

testAssociativity :: TestTree
testAssociativity = testPropertyNamed "Associativity" "testAssociativity" $ do
  property $ do
    x <- forAll genWD
    y <- forAll genWD
    z <- forAll genWD

    (x <> y) <> z === x <> (y <> z)

testIdentity :: TestTree
testIdentity = testPropertyNamed "Identity" "testIdentity" $ do
  property $ do
    x <- forAll genWD

    x === mempty <> x
    x === x <> mempty

testLeftBias :: TestTree
testLeftBias = testPropertyNamed "Left bias" "testLeftBias" $ do
  property $ do
    x <- forAll genWith
    y <- forAll genWith

    x === x <> y
    y === y <> x

testDisableNormalSubmonoid :: TestTree
testDisableNormalSubmonoid = testPropertyNamed desc name $ do
  property $ do
    x <- forAll genWith

    Disabled <> x === Disabled
    Disabled === x <> Disabled
  where
    desc = "Disable is a normal submonoid"
    name = "testDisableNormalSubmonoid"

testFoldable :: TestTree
testFoldable = testPropertyNamed "Foldable" "testFoldable" $ do
  property $ do
    x <- forAll genWD

    -- foldr f z t === appEndo (foldMap (Endo . f) t ) z
    let foldrResult = foldFn x
        endoResult = endoFn x

    foldrResult === endoResult
  where
    foldFn = foldr accFn start
    endoFn t = appEndo (foldMap (Endo . accFn) t) start

    start :: String
    start = "acc"

    accFn :: Int -> String -> String
    accFn i acc = show i ++ acc

testsFunctions :: TestTree
testsFunctions =
  testGroup
    "Functions"
    [ testsFromMaybe,
      testsToMaybe,
      testsFromWithDisabled
    ]

testsFromMaybe :: TestTree
testsFromMaybe = testPropertyNamed desc name $ do
  property $ do
    x <- forAll genInt

    With x === WD.fromMaybe (Just x)
    Without === WD.fromMaybe (Nothing @Int)
  where
    desc = "testsFromMaybe"
    name = "fromMaybe"

testsToMaybe :: TestTree
testsToMaybe = testPropertyNamed desc name $ do
  property $ do
    x <- forAll genInt

    Just x === WD.toMaybe (With x)
    Nothing === WD.toMaybe (Without @Int)
    Nothing === WD.toMaybe (Disabled @Int)
  where
    desc = "testsToMaybe"
    name = "toMaybe"

testsFromWithDisabled :: TestTree
testsFromWithDisabled = testPropertyNamed desc name $ do
  property $ do
    d <- forAll genInt
    e <- forAll genInt

    d === WD.fromWithDisabled d Disabled
    d === WD.fromWithDisabled d Without
    e === WD.fromWithDisabled d (With e)
  where
    desc = "fromWithDisabled"
    name = "testsFromWithDisabled"

genWD :: Gen (WithDisabled Int)
genWD =
  G.choice
    [ genWith,
      pure Without,
      pure Disabled
    ]

genWith :: Gen (WithDisabled Int)
genWith = With <$> genInt

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100
