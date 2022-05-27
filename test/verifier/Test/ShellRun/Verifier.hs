{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for verifying output.
module Test.ShellRun.Verifier
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
import Optics.Core (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.HUnit qualified as THU
import Prelude

-- | Newtype wrapper for 'Text' results.
newtype ResultText = MkResultText {getResultText :: Text}
  deriving (IsString, Show) via Text

makeFieldLabelsNoPrefix ''ResultText

-- | Newtype wrapper for expected 'Text' results.
newtype ExpectedText = MkExpectedText {getExpectedText :: Text}
  deriving (IsString, Show) via Text

makeFieldLabelsNoPrefix ''ExpectedText

-- | Newtype wrapper for unexpected 'Text' results.
newtype UnexpectedText = MkUnexpectedText {getUnexpectedText :: Text}
  deriving (IsString, Show) via Text

makeFieldLabelsNoPrefix ''UnexpectedText

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
  THU.assertBool err found
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
  THU.assertBool err (not found)
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: [ResultText] -> String
formatResults results = T.unpack lineSep
  where
    results' = fmap (view #getResultText) results
    lineSep = T.intercalate "\n" results'
