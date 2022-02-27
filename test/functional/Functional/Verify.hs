{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for verifying output.
module Functional.Verify
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

import Data.String (String)
import Data.Text qualified as T
import Functional.Prelude
import Test.Tasty.HUnit qualified as THU

-- | Newtype wrapper for 'Text' results.
newtype ResultText = MkResultText {getResultText :: Text}
  deriving (Show) via Text

makeFieldLabelsNoPrefix ''ResultText

-- | Newtype wrapper for expected 'Text' results.
newtype ExpectedText = MkExpectedText {getExpectedText :: Text}
  deriving (Show) via Text

makeFieldLabelsNoPrefix ''ExpectedText

-- | Newtype wrapper for unexpected 'Text' results.
newtype UnexpectedText = MkUnexpectedText {getUnexpectedText :: Text}
  deriving (Show) via Text

makeFieldLabelsNoPrefix ''UnexpectedText

-- | Verifies expected text is found.
verifyExpected :: List ResultText -> List ExpectedText -> Assertion
verifyExpected results = flip (verifyExpectedUnexpected results) []

-- | Verifies unexpected text is not found.
verifyUnexpected :: List ResultText -> List UnexpectedText -> Assertion
verifyUnexpected results = verifyExpectedUnexpected results []

-- | Verifies expected text is found and unexpected text is not found.
verifyExpectedUnexpected :: List ResultText -> List ExpectedText -> List UnexpectedText -> Assertion
verifyExpectedUnexpected results allExpected allUnexpected = allExpectedFound *> allUnexpectedNotFound
  where
    findExpected :: ExpectedText -> Assertion -> Assertion
    findExpected expected acc = findOneExpected results expected *> acc
    allExpectedFound = foldr findExpected (pure ()) allExpected

    findUnexpected :: UnexpectedText -> Assertion -> Assertion
    findUnexpected unexpected acc = findOneUnexpected results unexpected *> acc
    allUnexpectedNotFound = foldr findUnexpected (pure ()) allUnexpected

findOneExpected :: List ResultText -> ExpectedText -> Assertion
findOneExpected results (MkExpectedText expected) = do
  let found = foldr searchT False results
      err =
        "Could not find expected <"
          <> T.unpack expected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
  THU.assertBool err found
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = expected `T.isInfixOf` result || acc

findOneUnexpected :: List ResultText -> UnexpectedText -> Assertion
findOneUnexpected results (MkUnexpectedText unexpected) = do
  let found = foldr searchT False results
      err =
        "Found unexpected <"
          <> T.unpack unexpected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
  THU.assertBool err (not found)
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: List ResultText -> String
formatResults results = T.unpack lineSep
  where
    results' = fmap (view #getResultText) results
    lineSep = T.intercalate "\n" results'
