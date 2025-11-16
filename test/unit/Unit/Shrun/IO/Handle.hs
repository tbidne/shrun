module Unit.Shrun.IO.Handle (tests) where

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO.Handle
  ( ReadHandleResult
      ( ReadErr,
        ReadErrSuccess,
        ReadNoData,
        ReadSuccess
      ),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.IO.Handle"
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
      testRHRIdentity
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

genReadHandleResult :: Gen ReadHandleResult
genReadHandleResult =
  G.choice
    [ genReadSuccess,
      genReadErrSuccess,
      genReadErr,
      pure ReadNoData
    ]

genReadSuccess :: Gen ReadHandleResult
genReadSuccess = ReadSuccess <$> genUnlinedTexts

genReadErrSuccess :: Gen ReadHandleResult
genReadErrSuccess = ReadErrSuccess <$> genUnlinedTexts <*> genUnlinedTexts

genReadErr :: Gen ReadHandleResult
genReadErr = ReadErr <$> genUnlinedTexts

genUnlinedTexts :: Gen (NonEmpty UnlinedText)
genUnlinedTexts = ShrunText.unsafeFromTextNE <$> genText

genText :: Gen Text
genText = G.text (R.linearFrom 1 1 20) G.unicode
