-- | Property tests for Shrun.Legend.Internal.
module Unit.Props.Shrun.Configuration.Legend (props) where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Shrun.Configuration.Legend (LegendMap, linesToMap, translateCommands)
import Shrun.Data.Legend (KeyVal, unsafeKeyVal)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Test.Tasty (askOption)
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude

-- | Entry point for Shrun.Legend.Internal property tests.
props :: TestTree
props =
  testGroup
    "Shrun.Configuration.Legend"
    [ linesToMapProps,
      translateProps
    ]

linesToMapProps :: TestTree
linesToMapProps =
  testGroup
    "linesToMap"
    [ successProps,
      failureProps
    ]

successProps :: TestTree
successProps =
  askOption $ \(MkMaxRuns limit) ->
    testPropertyNamed "linesToMap success props" "successProps" $
      -- NOTE: This set to at least 1,000 as we have had test errors before
      -- that got through (i.e. duplicate keys) because 100 was not enough,
      -- and it's still fast.
      let limit' = max 1_000 limit
       in H.withTests limit' $
            H.property $ do
              commands <- H.forAll genGoodLines
              let result = linesToMap commands
              case result of
                Left err -> do
                  H.footnoteShow err
                  H.failure
                Right legend -> do
                  H.annotate "Unique keys in original list should match legend"
                  verifySize commands legend

failureProps :: TestTree
failureProps = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "linesToMap failure props" "failureProps" $
    H.withTests limit $
      H.property $ do
        pure ()

-- commands <- H.forAll genBadLines
-- let result = linesToMap commands
-- case result of
--   Left _ -> H.success
--   Right _ -> H.failure

verifySize :: List KeyVal -> LegendMap -> PropertyT IO ()
verifySize commands legend = do
  H.annotateShow commands
  let numUniqueKeys = length $ Set.fromList (fmap (view #key) commands)
      numLegendKeys = length $ Map.keys legend

  H.annotate $ "Commands: " <> show commands
  H.annotate $ "Legend: " <> show legend
  H.annotate $ "numUniqueKeys: " <> show numUniqueKeys
  H.annotate $ "numLegendKeys: " <> show numLegendKeys
  numLegendKeys === numUniqueKeys

genGoodLines :: MonadGen m => m (List KeyVal)
genGoodLines = do
  keyVals <- Gen.list range genGoodLine
  let (_, unique) = foldl' takeUnique (Set.empty, []) keyVals

  Gen.shuffle unique
  where
    range = Range.linearFrom 20 1 80
    takeUnique (foundKeys, newList) (MkGoodLine k v)
      | Set.member k foundKeys = (foundKeys, newList)
      | otherwise = (Set.insert k foundKeys, unsafeKeyVal k [v] : newList)

data GoodLine = MkGoodLine
  { gkey :: Text,
    gvalue :: Text
  }
  deriving stock (Show)

genGoodLine :: MonadGen m => m GoodLine
genGoodLine = MkGoodLine <$> genKey <*> genVal

genKey :: MonadGen m => m Text
genKey = Gen.filterT noSpaceOrEquals $ Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10
    noSpaceOrEquals = T.all (\c -> c /= ' ' && c /= '=')

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 10 1 30

translateProps :: TestTree
translateProps = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "translateCommands includes everything" "translateProps" $
    H.withTests limit $
      H.property $ do
        (legend, origCmds) <- H.forAll genLegendCommands
        let legendKeySet = Set.fromList $ Map.keys legend
            maybeFinalCmds = translateCommands legend origCmds

        case maybeFinalCmds of
          Left err -> do
            H.footnote $ "Received a LegendErr: " <> show err
            H.failure
          Right finalCmds -> do
            let finalCmdsSet = Set.fromList $ NESeq.toList $ fmap (view #command) finalCmds
                combinedKeySet = Set.union legendKeySet finalCmdsSet

            H.footnote $ "Final commands: " <> show finalCmdsSet
            H.footnote $ "Legend: " <> show legendKeySet
            noCommandsMissing combinedKeySet origCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: HashSet Text -> NonEmptySeq Text -> PropertyT IO ()
noCommandsMissing allKeys = void . traverse failIfMissing
  where
    failIfMissing cmd
      | Set.member cmd allKeys = pure ()
      | otherwise = do
          H.footnote $ "Missing command: " <> show cmd
          H.failure

genLegendCommands :: (GenBase m ~ Identity, MonadGen m) => m (LegendMap, NonEmptySeq Text)
genLegendCommands = (,) <$> genLegend <*> genCommands

-- In order to avoid cycles -- e.g. a -> b -> a -- we disallow all recursive
-- references i.e. a val is not allowed to reference a key at all.
-- This is a bit of a sledgehammer approach, as we certainly allow
-- (non-cyclic) recursive references in our application. That said,
-- this stronger "no references" rule is significantly simpler to enforce
-- here.
genLegend :: (GenBase m ~ Identity, MonadGen m) => m LegendMap
genLegend = do
  keyVals <- Gen.list range genKeyVal
  let keyVals' = fmap (\kv -> (kv ^. #key, kv ^. #val)) keyVals
      keySet = Set.fromList $ fmap fst keyVals'
      noCycles = foldl' (noKeyEqVal keySet) [] keyVals'
  pure $ Map.fromList noCycles
  where
    range = Range.linearFrom 0 0 80
    noKeyEqVal ks acc p@(_, v)
      | cmdInSet ks v = acc
      | otherwise = p : acc
    -- Split RHS value into all cmds, reject if any reference a key
    cmdInSet s = any (`Set.member` s)

genKeyVal :: (GenBase m ~ Identity, MonadGen m) => m KeyVal
genKeyVal = do
  k <- genKey
  -- Reject a=a key/val pairs, intended to avoid cycles. Technically
  -- unnecessary as genLegend rejects all recursive references,
  -- not just trivial a=a. But we might as well prevent ourselves from
  -- constructing these trivial bad values in the first place since:
  --  1. it's easy
  --  2. makes our test more robust (tests more values)
  --  3. The performance hit is negligible
  v <- Gen.filter (/= k) genVal
  pure $ unsafeKeyVal k [v]

genCommands :: MonadGen m => m (NonEmptySeq Text)
genCommands = NESeq.unsafeFromList <$> Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50
