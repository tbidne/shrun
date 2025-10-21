{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}

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
    [ testLaws,
      testsFunctions
    ]

testLaws :: TestTree
testLaws =
  testGroup
    "Alternative Laws"
    [ testAssociativity,
      testIdentity,
      testLeftBias
    ]

testAssociativity :: TestTree
testAssociativity = testPropertyNamed "Associativity" "testAssociativity" $ do
  property $ do
    x <- forAll genWD
    y <- forAll genWD
    z <- forAll genWD

    ((x <|> y) <|> z) === (x <|> (y <|> z))

testIdentity :: TestTree
testIdentity = testPropertyNamed "Identity" "testIdentity" $ do
  property $ do
    x <- forAll genWD

    x === (empty <|> x)
    x === x <|> empty

testLeftBias :: TestTree
testLeftBias = testPropertyNamed "Left bias" "testLeftBias" $ do
  property $ do
    x <- forAll genWith
    y <- forAll genWith

    x === (x <|> y)
    y === (y <|> x)

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

genWD :: Gen (WithDisabled Int)
genWD =
  G.choice
    [ genWith,
      pure Disabled
    ]

genWith :: Gen (WithDisabled Int)
genWith = With <$> genInt

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100
