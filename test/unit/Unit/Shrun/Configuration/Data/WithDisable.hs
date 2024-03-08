{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

module Unit.Shrun.Configuration.Data.WithDisable
  ( tests,
  )
where

import Data.Foldable (Foldable (foldMap))
import Data.Monoid (Endo (Endo, appEndo))
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Configuration.Data.WithDisable (WithDisable (Disabled, With))
import Shrun.Configuration.Data.WithDisable qualified as WD
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Data.WithDisable"
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

    accFn :: Maybe Int -> String -> String
    accFn i acc = show i ++ acc

testsFunctions :: TestTree
testsFunctions =
  testGroup
    "Functions"
    [ testsDefaultIfDisabled,
      testsEmptyIfDisabled,
      testsAlternativeDefault,
      testAlternativeEmpty
    ]

testsEmptyIfDisabled :: TestTree
testsEmptyIfDisabled = testPropertyNamed desc name $ do
  property $ do
    x <- forAll genMaybeInt

    x === WD.emptyIfDisabled (With x)

    Nothing === WD.emptyIfDisabled (Disabled @(Maybe Int))
  where
    desc = "testsEmptyIfDisabled"
    name = "emptyIfDisabled"

testsDefaultIfDisabled :: TestTree
testsDefaultIfDisabled = testPropertyNamed desc name $ do
  property $ do
    d <- forAll genInt
    e <- forAll genInt

    d === WD.defaultIfDisabled d (Disabled @(Maybe Int))
    d === WD.defaultIfDisabled d (With Nothing)
    e === WD.defaultIfDisabled d (With $ Just e)
  where
    desc = "defaultIfDisabled"
    name = "testsDefaultIfDisabled"

testsAlternativeDefault :: TestTree
testsAlternativeDefault = testPropertyNamed desc name $ do
  property $ do
    d <- forAll genInt
    x <- forAll genMaybeInt

    d === WD.alternativeDefault d Disabled x

    d === WD.alternativeDefault d (With Nothing) Nothing
  where
    desc = "alternativeDefault"
    name = "testsAlternativeDefault"

testAlternativeEmpty :: TestTree
testAlternativeEmpty = testPropertyNamed desc name $ do
  property $ do
    y <- forAll genMaybeInt
    Nothing === WD.alternativeEmpty Disabled y

    wd@(With x) <- forAll genWith
    (x <|> y) === WD.alternativeEmpty wd y
  where
    desc = "alternativeEmpty"
    name = "testAlternativeEmpty"

genWD :: Gen (WithDisable (Maybe Int))
genWD =
  G.choice
    [ genWith,
      pure $ With Nothing,
      pure Disabled
    ]

genWith :: Gen (WithDisable (Maybe Int))
genWith = With <$> genJustInt

genMaybeInt :: Gen (Maybe Int)
genMaybeInt =
  G.choice
    [ genJustInt,
      pure Nothing
    ]

genJustInt :: Gen (Maybe Int)
genJustInt = Just <$> genInt

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100
