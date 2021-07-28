-- | Provides functionality for verifying output.
module Verify
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

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec (Expectation)
import Test.Hspec qualified as Hspec

-- | Newtype wrapper for 'Text' results.
newtype ResultText = MkResultText {getResultText :: Text}
  deriving (Show) via Text

-- | Newtype wrapper for expected 'Text' results.
newtype ExpectedText = MkExpectedText {getExpectedText :: Text}
  deriving (Show) via Text

-- | Newtype wrapper for unexpected 'Text' results.
newtype UnexpectedText = MkUnexpectedText {getUnexpectedText :: Text}
  deriving (Show) via Text

-- | Verifies expected text is found.
verifyExpected :: [ResultText] -> [ExpectedText] -> Expectation
verifyExpected results = flip (verifyExpectedUnexpected results) []

-- | Verifies unexpected text is not found.
verifyUnexpected :: [ResultText] -> [UnexpectedText] -> Expectation
verifyUnexpected results = verifyExpectedUnexpected results []

-- | Verifies expected text is found and unexpected text is not found.
verifyExpectedUnexpected :: [ResultText] -> [ExpectedText] -> [UnexpectedText] -> Expectation
verifyExpectedUnexpected results allExpected allUnexpected = allExpectedFound *> allUnexpectedNotFound
  where
    findExpected :: ExpectedText -> Expectation -> Expectation
    findExpected expected acc = findOneExpected results expected *> acc
    allExpectedFound = foldr findExpected (pure ()) allExpected

    findUnexpected :: UnexpectedText -> Expectation -> Expectation
    findUnexpected unexpected acc = findOneUnexpected results unexpected *> acc
    allUnexpectedNotFound = foldr findUnexpected (pure ()) allUnexpected

findOneExpected :: [ResultText] -> ExpectedText -> Expectation
findOneExpected results (MkExpectedText expected) = do
  let found = foldr searchT False results
  if not found
    then do
      Hspec.expectationFailure $
        "Could not find expected <"
          <> T.unpack expected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
    else pure ()
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = expected `T.isInfixOf` result || acc

findOneUnexpected :: [ResultText] -> UnexpectedText -> Expectation
findOneUnexpected results (MkUnexpectedText unexpected) = do
  let found = foldr searchT False results
  if found
    then
      Hspec.expectationFailure $
        "Found unexpected <"
          <> T.unpack unexpected
          <> "> in output: \n<"
          <> formatResults results
          <> ">"
    else pure ()
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: [ResultText] -> String
formatResults results = T.unpack lineSep
  where
    results' = fmap getResultText results
    lineSep = T.intercalate "\n" results'
