module Unit.Shrun.Data.Result (tests) where

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Data.Result"
    [ testLaws
    ]

testLaws :: TestTree
testLaws =
  testGroup
    "Laws"
    [ testAssociativity,
      testIdentity,
      testLeftErrBias,
      testErrAbsorbs
    ]

testAssociativity :: TestTree
testAssociativity = testPropertyNamed "Associativity" "testAssociativity" $ do
  property $ do
    x <- forAll genResult
    y <- forAll genResult
    z <- forAll genResult

    ((x <> y) <> z) === (x <> (y <> z))

testIdentity :: TestTree
testIdentity = testPropertyNamed "Identity" "testIdentity" $ do
  property $ do
    x <- forAll genResult

    x === (mempty <> x)
    x === x <> mempty

testLeftErrBias :: TestTree
testLeftErrBias = testPropertyNamed "Left Err bias" "testLeftErrBias" $ do
  property $ do
    x <- forAll genErr
    y <- forAll genErr

    x === (x <> y)
    y === (y <> x)

testErrAbsorbs :: TestTree
testErrAbsorbs = testPropertyNamed "Err absorbs" "testErrAbsorbs" $ do
  property $ do
    x <- forAll genErr
    y <- forAll genOk

    x === (x <> y)
    x === (y <> x)

genResult :: Gen (Result Int Text)
genResult =
  G.choice
    [ genOk,
      genErr
    ]

genOk :: Gen (Result Int Text)
genOk = Ok <$> genText

genErr :: Gen (Result Int Text)
genErr = Err <$> genInt

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100

genText :: Gen Text
genText = G.text (R.linearFrom 0 0 20) G.unicode
