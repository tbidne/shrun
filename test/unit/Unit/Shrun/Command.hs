module Unit.Shrun.Command (tests) where

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Command
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Command"
    [ testLaws
    ]

testLaws :: TestTree
testLaws =
  testGroup
    "PredecessorResult Laws"
    [ testAssociativity,
      testIdentity,
      testLeftBias,
      testErrorAbsorbs
    ]

testAssociativity :: TestTree
testAssociativity = testPropertyNamed "Associativity" "testAssociativity" $ do
  property $ do
    x <- forAll genPredecessorResult
    y <- forAll genPredecessorResult
    z <- forAll genPredecessorResult

    ((x <> y) <> z) === (x <> (y <> z))

testIdentity :: TestTree
testIdentity = testPropertyNamed "Identity" "testIdentity" $ do
  property $ do
    x <- forAll genPredecessorResult

    x === (mempty <> x)
    x === x <> mempty

testLeftBias :: TestTree
testLeftBias = testPropertyNamed "Left bias" "testLeftBias" $ do
  property $ do
    u1 <- forAll genUnfinished
    u2 <- forAll genUnfinished

    f1 <- forAll genFailure
    f2 <- forAll genFailure

    u1 === (u1 <> u2)
    u2 === (u2 <> u1)

    f1 === (f1 <> f2)
    f2 === (f2 <> f1)

testErrorAbsorbs :: TestTree
testErrorAbsorbs = testPropertyNamed "Error absorbs" "testErrorAbsorbs" $ do
  property $ do
    x <- forAll genNonSuccess

    x === (x <> PredecessorSuccess)
    x === (PredecessorSuccess <> x)

genPredecessorResult :: Gen PredecessorResult
genPredecessorResult =
  G.choice
    [ genUnfinished,
      pure PredecessorSuccess,
      genFailure
    ]

genNonSuccess :: Gen PredecessorResult
genNonSuccess =
  G.choice
    [ genUnfinished,
      genFailure
    ]

genUnfinished :: Gen PredecessorResult
genUnfinished = PredecessorUnfinished <$> genInt

genFailure :: Gen PredecessorResult
genFailure = PredecessorFailure <$> G.enumBounded <*> genInt

genInt :: Gen Int
genInt = G.integral $ R.linearFrom 0 0 100
