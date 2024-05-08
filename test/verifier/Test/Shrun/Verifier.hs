{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for verifying output.
module Test.Shrun.Verifier
  ( -- * Types for verifying output
    ResultText (..),
    ExpectedText (..),
    UnexpectedText (..),

    -- * Verifying functions
    verifyExpected,
    verifyUnexpected,
    verifyExpectedUnexpected,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core (An_Iso, LabelOptic (labelOptic), iso, view)
import Test.Tasty.HUnit (Assertion, assertBool)
import Prelude

-- | Newtype wrapper for 'Text' results.
newtype ResultText = MkResultText {unResultText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unResultText" k ResultText ResultText a b
  where
  labelOptic = iso (\(MkResultText x) -> x) MkResultText
  {-# INLINE labelOptic #-}

-- | Newtype wrapper for expected 'Text' results.
newtype ExpectedText = MkExpectedText {unExpectedText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unExpectedText" k ExpectedText ExpectedText a b
  where
  labelOptic = iso (\(MkExpectedText x) -> x) MkExpectedText
  {-# INLINE labelOptic #-}

-- | Newtype wrapper for unexpected 'Text' results.
newtype UnexpectedText = MkUnexpectedText {unUnexpectedText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unUnexpectedText" k UnexpectedText UnexpectedText a b
  where
  labelOptic = iso (\(MkUnexpectedText x) -> x) MkUnexpectedText
  {-# INLINE labelOptic #-}

-- | Verifies expected text is found.
verifyExpected :: [ResultText] -> [ExpectedText] -> Assertion
verifyExpected results = flip (verifyExpectedUnexpected results) []

-- | Verifies unexpected text is not found.
verifyUnexpected :: [ResultText] -> [UnexpectedText] -> Assertion
verifyUnexpected results = verifyExpectedUnexpected results []

-- | Verifies expected text is found and unexpected text is not found.
verifyExpectedUnexpected :: [ResultText] -> [ExpectedText] -> [UnexpectedText] -> Assertion
verifyExpectedUnexpected results allExpected allUnexpected = allExpectedFound *> allUnexpectedNotFound
  where
    findExpected :: ExpectedText -> Assertion -> Assertion
    findExpected expected acc = findOneExpected results expected *> acc
    allExpectedFound = foldr findExpected (pure ()) allExpected

    findUnexpected :: UnexpectedText -> Assertion -> Assertion
    findUnexpected unexpected acc = findOneUnexpected results unexpected *> acc
    allUnexpectedNotFound = foldr findUnexpected (pure ()) allUnexpected

findOneExpected :: [ResultText] -> ExpectedText -> Assertion
findOneExpected results (MkExpectedText expected) = do
  let found = foldr searchT False results
      err =
        "Could not find expected <"
          <> T.unpack expected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
  assertBool err found
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = expected `T.isInfixOf` result || acc

findOneUnexpected :: [ResultText] -> UnexpectedText -> Assertion
findOneUnexpected results (MkUnexpectedText unexpected) = do
  let found = foldr searchT False results
      err =
        "Found unexpected <"
          <> T.unpack unexpected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
  assertBool err (not found)
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: [ResultText] -> String
formatResults results = T.unpack lineSep
  where
    results' = fmap (view #unResultText) results
    lineSep = T.intercalate "\n" results'
