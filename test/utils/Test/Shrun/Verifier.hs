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
    verifyExpectedOrder,
    verifyUnexpected,
    verifyExpectedUnexpected,

    -- ** Some
    verifySomeExpected,
  )
where

import Control.Monad (void, when)
import Data.Foldable qualified as F
#if MIN_VERSION_base(4, 20, 0)
import Data.List (List)
#endif
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.HUnit (Assertion, assertFailure)
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

-- | Verifies that at least one expected is found in the results.
verifySomeExpected ::
  List ResultText ->
  List ExpectedText ->
  Assertion
verifySomeExpected results someExpected = F.for_ someExpectedFound assertFailure
  where
    findExpected :: ExpectedText -> Maybe String -> Maybe String
    findExpected expected acc = findOneExpectedStr results expected *> acc
    someExpectedFound = foldr findExpected (Just err) someExpected

    err =
      mconcat
        [ "Did not find any expected text:",
          mconcat $ fmap ((\t -> "\n - '" <> t <> "'") . T.unpack . (.unExpectedText)) someExpected,
          "\n\nin output:\n\n",
          formatResults results
        ]

-- | Verifies that the expected text occurs exactly once, in the given order.
verifyExpectedOrder ::
  List ResultText ->
  List ExpectedText ->
  Assertion
verifyExpectedOrder results = void . allExpectedFound ("<none>", -1)
  where
    findExpected :: ExpectedText -> (ExpectedText, Int) -> IO (ExpectedText, Int)
    findExpected expected@(MkExpectedText e) acc = do
      let (MkExpectedText prevExpected, prevIdx) = acc
      newIdx <- findOneExpectedIdx results expected

      when (prevIdx >= newIdx) $ do
        assertFailure $
          mconcat
            [ "Expected text '",
              T.unpack e,
              "' -- found at index ",
              show newIdx,
              " -- is <= the index ",
              show prevIdx,
              " for the previously found:\n\n",
              T.unpack prevExpected,
              "\n\nin output:\n\n",
              formatResults results
            ]

      pure (expected, newIdx)

    -- Direct recursion to make order more explicit.
    allExpectedFound _ [] = pure ()
    allExpectedFound acc (e : es) = do
      newAcc <- findExpected e acc
      allExpectedFound newAcc es

findOneExpected :: List ResultText -> ExpectedText -> Assertion
findOneExpected results = findOneExpectedN results . (1,)

findOneExpectedStr :: List ResultText -> ExpectedText -> Maybe String
findOneExpectedStr results = findOneExpectedNStr results . (1,)

findOneExpectedN :: List ResultText -> (Int, ExpectedText) -> Assertion
findOneExpectedN results es =
  F.for_ (findOneExpectedNStr results es) assertFailure

findOneExpectedNStr :: List ResultText -> (Int, ExpectedText) -> Maybe String
findOneExpectedNStr results (numExpected, MkExpectedText expected) =
  let numHits = F.foldl' searchT 0 results
   in if numHits == numExpected
        then Nothing
        else
          Just $
            mconcat
              [ "Expected text '",
                T.unpack expected,
                "' ",
                show numExpected,
                " times, found ",
                show numHits,
                " in output:\n\n",
                formatResults results
              ]
  where
    searchT :: Int -> ResultText -> Int
    searchT !acc (MkResultText result) =
      if expected `T.isInfixOf` result
        then acc + 1
        else acc

findOneExpectedIdx :: List ResultText -> ExpectedText -> IO Int
findOneExpectedIdx results (MkExpectedText expected) = do
  let (_, mFoundIdx) = go (0, Nothing) results

  case mFoundIdx of
    Nothing ->
      assertFailure $
        mconcat
          [ "Did not find expected text '",
            T.unpack expected,
            "' in output:\n\n",
            formatResults results
          ]
    Just (foundIdx, numHits) ->
      if numHits == 1
        then pure foundIdx
        else
          assertFailure $
            mconcat
              [ "Expected text '",
                T.unpack expected,
                "' 1 time, found ",
                show numHits,
                " in output:\n\n",
                formatResults results
              ]
  where
    -- Explicit recursion for clear order.
    go acc [] = acc
    go acc (r : rs) = go (searchT r acc) rs

    searchT :: ResultText -> (Int, Maybe (Int, Int)) -> (Int, Maybe (Int, Int))
    searchT (MkResultText result) (!idx, mFoundIdx) =
      if expected `T.isInfixOf` result
        then (idx + 1, addHit idx mFoundIdx)
        else (idx + 1, mFoundIdx)

    addHit idx Nothing = Just (idx, 1)
    addHit _ (Just (idx, numHits)) = Just (idx, numHits + 1)

findOneUnexpected :: List ResultText -> UnexpectedText -> Assertion
findOneUnexpected results e =
  F.for_ (findOneUnexpectedStr results e) assertFailure

findOneUnexpectedStr :: List ResultText -> UnexpectedText -> Maybe String
findOneUnexpectedStr results (MkUnexpectedText unexpected) =
  let found = foldr searchT False results
      err =
        mconcat
          [ "Found unexpected '",
            T.unpack unexpected,
            "' in output:\n\n",
            formatResults results
          ]
   in if not found
        then Nothing
        else Just err
  where
    searchT :: ResultText -> Bool -> Bool
    searchT (MkResultText result) acc = unexpected `T.isInfixOf` result || acc

formatResults :: List ResultText -> String
formatResults =
  T.unpack
    . T.intercalate "\n"
    . zipWith (\i r -> showt i <> ". " <> r) [0 :: Int ..]
    . fmap (.unResultText)

showt :: (Show a) => a -> Text
showt = T.pack . show
