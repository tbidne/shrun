-- | Property tests for ShellRun.Legend.Internal.
module Unit.Props.ShellRun.Configuration.Legend (props) where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ShellRun.Configuration.Legend (LegendMap, linesToMap, translateCommands)
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import Test.Tasty (askOption)
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude

-- | Entry point for ShellRun.Legend.Internal property tests.
props :: TestTree
props =
  testGroup
    "ShellRun.Configuration.Legend"
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
        commands <- H.forAll genBadLines
        let result = linesToMap commands
        case result of
          Left _ -> H.success
          Right _ -> H.failure

verifySize :: List Text -> LegendMap -> PropertyT IO ()
verifySize commands legend = do
  H.annotateShow commands
  let noComments = filter (not . T.isPrefixOf "#") commands
      textKeys = fmap getKey noComments
      numUniqueKeys = length $ Set.fromList textKeys
      numLegendKeys = length $ Map.keys legend

  H.annotate $ "Commands: " <> show commands
  H.annotate $ "Legend: " <> show legend
  H.annotate $ "numUniqueKeys: " <> show numUniqueKeys
  H.annotate $ "numLegendKeys: " <> show numLegendKeys
  numLegendKeys === numUniqueKeys
  where
    getKey = fst . T.break (== '=')

genGoodLines :: MonadGen m => m (List Text)
genGoodLines = do
  keyVals <- Gen.list range genGoodLine
  comments <- Gen.list range genComment
  let (_, unique) = foldl' takeUnique (Set.empty, []) keyVals

  Gen.shuffle (unique <> comments)
  where
    range = Range.linearFrom 20 1 80
    takeUnique (foundKeys, newList) (MkGoodLine k v)
      | Set.member k foundKeys = (foundKeys, newList)
      | otherwise = (Set.insert k foundKeys, k <> "=" <> v : newList)

genComment :: MonadGen m => m Text
genComment = do
  c <- Gen.text range Gen.latin1
  pure $ "#" <> c
  where
    range = Range.linearFrom 20 1 80

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

genBadLines :: MonadGen m => m (List Text)
genBadLines = Gen.shuffle =<< (:) <$> genBadLine <*> genGoodLines

-- Since we have the format 'key=val' where val can also include '=', the only
-- way a line can be "bad" is if:
--   1. non-empty
--   2. not a comment (does not start with #)
--   3. has no '='
genBadLine :: MonadGen m => m Text
genBadLine = Gen.filterT noEquals $ Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10
    noEquals t = T.head t /= '#' && T.all (/= '=') t

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
  let keySet = Set.fromList $ fmap fst keyVals
  let noCycles = foldl' (noKeyEqVal keySet) [] keyVals
  pure $ Map.fromList noCycles
  where
    range = Range.linearFrom 0 0 80
    noKeyEqVal ks acc p@(_, v)
      | cmdInSet ks v = acc
      | otherwise = p : acc
    -- Split RHS value into all cmds, reject if any reference a key
    cmdInSet s cmd =
      let cmds = T.splitOn ",," cmd
       in any (`Set.member` s) cmds

genKeyVal :: (GenBase m ~ Identity, MonadGen m) => m (Tuple2 Text Text)
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
  pure (k, v)

genCommands :: MonadGen m => m (NonEmptySeq Text)
genCommands = NESeq.unsafeFromList <$> Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50
