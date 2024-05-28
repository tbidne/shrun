module Unit.Shrun.IO.Types (tests) where

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO.Types
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.IO.Types"
    [ readHandleResultTests
    ]

readHandleResultTests :: TestTree
readHandleResultTests =
  testGroup
    "ReadHandleResult"
    [ testRHRLaws
    ]

testRHRLaws :: TestTree
testRHRLaws =
  testGroup
    "Laws"
    [ testRHRAssociativity,
      testRHRIdentity,
      testRHRLeftBias,
      testReadSuccessNormalSubmonoid
    ]

testRHRAssociativity :: TestTree
testRHRAssociativity = testPropertyNamed "Associativity" "testRHRAssociativity" $ do
  property $ do
    x <- forAll genReadHandleResult
    y <- forAll genReadHandleResult
    z <- forAll genReadHandleResult

    (x <> y) <> z === x <> (y <> z)

testRHRIdentity :: TestTree
testRHRIdentity = testPropertyNamed "Identity" "testRHRIdentity" $ do
  property $ do
    x <- forAll genReadHandleResult

    x === mempty <> x
    x === x <> mempty

testRHRLeftBias :: TestTree
testRHRLeftBias = testPropertyNamed "Left bias" "testRHRLeftBias" $ do
  property $ do
    x <- forAll genReadSuccess
    y <- forAll genReadSuccess

    x === x <> y
    y === y <> x

testReadSuccessNormalSubmonoid :: TestTree
testReadSuccessNormalSubmonoid = testPropertyNamed desc name $ do
  property $ do
    s <- forAll genReadSuccess
    x <- forAll genNoSuccess

    s === s <> x
    s === x <> s
  where
    desc = "ReadSuccess is a normal submonoid"
    name = "testReadSuccessNormalSubmonoid"

genReadHandleResult :: Gen ReadHandleResult
genReadHandleResult =
  G.choice
    [ genReadSuccess,
      genReadErr,
      pure ReadNoData
    ]

genReadSuccess :: Gen ReadHandleResult
genReadSuccess = ReadSuccess <$> genUnlinedTexts

genReadErr :: Gen ReadHandleResult
genReadErr = ReadErr <$> genUnlinedTexts

genNoSuccess :: Gen ReadHandleResult
genNoSuccess = G.choice [genReadErr, pure ReadNoData]

genUnlinedTexts :: Gen (List UnlinedText)
genUnlinedTexts = ShrunText.fromText <$> genText

genText :: Gen Text
genText = G.text (R.linearFrom 1 1 20) G.unicode
