{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for verifying output.
module Test.Shrun.Verifier
  ( -- * Types for verifying output
    ResultText (..),
    ExpectedText (..),
    UnexpectedText (..),

    -- * Verifying functions
    verifyExpected,
    verifyExpectedN,
    verifyUnexpected,
    verifyExpectedUnexpected,
  )
where

#if MIN_VERSION_base(4, 20, 0)
import Data.List (List)
#else
import Data.Foldable (Foldable (foldl'))
#endif
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)
import Prelude

#if !MIN_VERSION_base(4, 20, 0)
-- | Alias for [].
type List = []
#endif

-- | Newtype wrapper for 'Text' results.
newtype ResultText = MkResultText {unResultText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

-- | Newtype wrapper for expected 'Text' results.
newtype ExpectedText = MkExpectedText {unExpectedText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

-- | Newtype wrapper for unexpected 'Text' results.
newtype UnexpectedText = MkUnexpectedText {unUnexpectedText :: Text}
  deriving (IsString, Monoid, Semigroup, Show) via Text

-- | Verifies expected text is found.
verifyExpected :: List ResultText -> List ExpectedText -> Assertion
verifyExpected results = flip (verifyExpectedUnexpected results) []

verifyExpectedN :: List ResultText -> List (Int, ExpectedText) -> Assertion
verifyExpectedN results allExpected = allExpectedFound
  where
    findExpected :: (Int, ExpectedText) -> Assertion -> Assertion
    findExpected expected acc = findOneExpectedN results expected *> acc
    allExpectedFound = foldr findExpected (pure ()) allExpected

-- | Verifies unexpected text is not found.
verifyUnexpected :: List ResultText -> List UnexpectedText -> Assertion
verifyUnexpected results = verifyExpectedUnexpected results []

-- | Verifies expected text is found and unexpected text is not found.
verifyExpectedUnexpected ::
  List ResultText ->
  List ExpectedText ->
  List UnexpectedText ->
  Assertion
verifyExpectedUnexpected results allExpected allUnexpected =
  allExpectedFound *> allUnexpectedNotFound
  where
    findExpected :: ExpectedText -> Assertion -> Assertion
    findExpected expected acc = findOneExpected results expected *> acc
    allExpectedFound = foldr findExpected (pure ()) allExpected

    findUnexpected :: UnexpectedText -> Assertion -> Assertion
    findUnexpected unexpected acc = findOneUnexpected results unexpected *> acc
    allUnexpectedNotFound = foldr findUnexpected (pure ()) allUnexpected

findOneExpected :: List ResultText -> ExpectedText -> Assertion
findOneExpected results = findOneExpectedN results . (1,)

findOneExpectedN :: List ResultText -> (Int, ExpectedText) -> Assertion
findOneExpectedN results (numExpected, MkExpectedText expected) = do
  let numHits = foldl' searchT 0 results

  if numHits == numExpected
    then pure ()
    else
      assertFailure $
        mconcat
          [ "Expected text <",
            T.unpack expected,
            "> ",
            show numExpected,
            " times, found ",
            show numHits,
            " in output: \n<",
            formatResults results,
            ">"
          ]
  where
    searchT :: Int -> ResultText -> Int
    searchT !acc (MkResultText result) =
      if expected `T.isInfixOf` result
        then acc + 1
        else acc

findOneUnexpected :: List ResultText -> UnexpectedText -> Assertion
findOneUnexpected results (MkUnexpectedText unexpected) = do
  let found = foldr searchT False results
      err =
        mconcat
          [ "Found unexpected <",
            T.unpack unexpected,
            "> in output: \n<",
            formatResults results,
            ">"
          ]
  assertBool err (not found)
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: List ResultText -> String
formatResults results = T.unpack lineSep
  where
    results' = fmap (.unResultText) results
    lineSep = T.intercalate "\n" results'
