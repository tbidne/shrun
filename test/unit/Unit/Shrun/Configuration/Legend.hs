{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

-- | Tests for Shrun.Configuration.Legend.
module Unit.Shrun.Configuration.Legend (tests) where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import GHC.Exts qualified as Exts
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Shrun.Command.Types
  ( CommandP (MkCommandP),
    CommandP1,
    fromPositive,
  )
import Shrun.Configuration.Data.Graph
  ( EdgeArgs (EdgeArgsSequential),
    Edges,
  )
import Shrun.Configuration.Legend
  ( CyclicKeyError (MkCyclicKeyError),
    DuplicateKeyError (MkDuplicateKeyError),
    LegendMap,
  )
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml.Legend (KeyVal, unsafeKeyVal)
import Shrun.Utils (indexPos)
import Unit.Prelude

-- | Entry point for Shrun.Legend.Internal property tests.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Legend"
    [ linesToMapSuccessProps,
      linesToMapSpecs,
      translateProps,
      translateSpecs
    ]

linesToMapSuccessProps :: TestTree
linesToMapSuccessProps =
  testProp "linesToMap success props" "successProps" $ do
    commands <- forAll genGoodLines
    let result = Legend.linesToMap commands
    case result of
      Left err -> do
        footnoteShow err
        failure
      Right legend -> do
        annotate "Unique keys in original list should match legend"
        verifySize commands legend

verifySize :: Seq KeyVal -> LegendMap -> PropertyT IO ()
verifySize commands legend = do
  annotateShow commands
  let numUniqueKeys = length $ (Set.fromList . toList) (fmap (view #key) commands)
      numLegendKeys = length $ Map.keys legend

  annotate $ "Commands: " <> show commands
  annotate $ "Legend: " <> show legend
  annotate $ "numUniqueKeys: " <> show numUniqueKeys
  annotate $ "numLegendKeys: " <> show numLegendKeys
  numLegendKeys === numUniqueKeys

genGoodLines :: (MonadGen m) => m (Seq KeyVal)
genGoodLines = do
  keyVals <- Gen.list range genGoodLine
  let (_, unique) = foldl' takeUnique (Set.empty, []) keyVals

  listToSeq <$> Gen.shuffle unique
  where
    range = Range.linearFrom 20 1 80
    takeUnique (foundKeys, newList) (MkGoodLine k v)
      | Set.member k foundKeys = (foundKeys, newList)
      | otherwise = (Set.insert k foundKeys, unsafeKeyVal Nothing k [v] : newList)

data GoodLine = MkGoodLine
  { gkey :: Text,
    gvalue :: Text
  }
  deriving stock (Show)

genGoodLine :: (MonadGen m) => m GoodLine
genGoodLine = MkGoodLine <$> genKey <*> genVal

genKey :: (MonadGen m) => m Text
genKey = Gen.filterT noSpaceOrEquals $ Gen.text range Gen.latin1
  where
    range = Range.linearFrom 5 1 10
    noSpaceOrEquals = T.all (\c -> c /= ' ' && c /= '=')

genVal :: (MonadGen m) => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 10 1 30

translateProps :: TestTree
translateProps =
  testPropertyNamed "translateCommands includes everything" "translateProps"
    $ property
    $ do
      (legend, origCmds) <- forAll genLegendCommands
      let legendKeySet = Set.fromList $ Map.keys legend
          maybeFinalCmds = Legend.translateCommands legend origCmds Nothing

      case maybeFinalCmds of
        Left err -> do
          footnote $ "Received a LegendErr: " <> show err
          failure
        Right (finalCmds, _) -> do
          let finalCmdsSet = Set.fromList $ toList $ fmap (view #command) finalCmds
              combinedKeySet = Set.union legendKeySet finalCmdsSet

          footnote $ "Final commands: " <> show finalCmdsSet
          footnote $ "Legend: " <> show legendKeySet
          noCommandsMissing combinedKeySet origCmds
          commandsIndexedOrder origCmds finalCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: HashSet Text -> NESeq Text -> PropertyT IO ()
noCommandsMissing allKeys = void . traverse failIfMissing
  where
    failIfMissing cmd
      | Set.member cmd allKeys = pure ()
      | otherwise = do
          footnote $ "Missing command: " <> show cmd
          failure

commandsIndexedOrder :: NESeq Text -> NESeq CommandP1 -> PropertyT IO ()
commandsIndexedOrder origCmds finalCmds = do
  for_ (indexPos (NESeq.zip origCmds finalCmds)) $ \(idx, (orig, final)) -> do
    annotateShow idx
    annotateShow orig
    annotateShow final

    -- Verifies that final commands are all indexed according to a unique
    -- nat in [1 .. numCmds + 1]
    fromPositive idx === final ^. #index

    -- Relies on the fact that commands do not reference each other! We do
    -- allow keys, though, so we have to relax the check below.
    let isCmd = orig == final ^. #command
        isKey = Just orig == final ^. #key
    assert $ isCmd || isKey

genLegendCommands :: (GenBase m ~ Identity, MonadGen m) => m (LegendMap, NESeq Text)
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
  let keyVals' = fmap (\kv -> (kv ^. #key, (kv ^. #val, kv ^. #edges))) keyVals
      keySet = Set.fromList $ fmap fst keyVals'
      noCycles = foldl' (noKeyEqVal keySet) [] keyVals'
  pure $ Map.fromList noCycles
  where
    range = Range.linearFrom 0 0 80
    noKeyEqVal ks acc p@(_, (v, _))
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
  pure $ unsafeKeyVal Nothing k [v]

genCommands :: (MonadGen m) => m (NESeq Text)
genCommands = unsafeListToNESeq <$> Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: (MonadGen m) => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50

translateSpecs :: TestTree
translateSpecs =
  testGroup
    "translateCommands"
    [ translateOneCmd,
      returnsNonMapCmd,
      returnsRecursiveCmds,
      returnsRecursiveAndOtherCmds,
      noSplitNonKeyCmd,
      cycleCmdFail,
      translateEdgesSpecs
    ]

translateOneCmd :: TestTree
translateOneCmd = testCase "Should translate one command" $ do
  result <- translateCommandsSuccess legendMap ("one" :<|| [])
  let expected = MkCommandP (mkIdx 1) (Just "one") "cmd1" :<|| []
  expected @=? result

returnsNonMapCmd :: TestTree
returnsNonMapCmd = testCase "Should return non-map command" $ do
  result <- translateCommandsSuccess legendMap ("other" :<|| [])
  let expected = MkCommandP (mkIdx 1) Nothing "other" :<|| []
  expected @=? result

returnsRecursiveCmds :: TestTree
returnsRecursiveCmds = testCase "Should return recursive commands" $ do
  result <- translateCommandsSuccess legendMap ("all" :<|| [])
  let expected =
        MkCommandP (mkIdx 1) (Just "one") "cmd1"
          :<|| [ MkCommandP (mkIdx 2) (Just "two") "cmd2",
                 MkCommandP (mkIdx 3) Nothing "cmd3"
               ]
  expected @=? result

returnsRecursiveAndOtherCmds :: TestTree
returnsRecursiveAndOtherCmds = testCase "Should return recursive commands and other" $ do
  result <- translateCommandsSuccess legendMap ("all" :<|| ["other"])
  let expected =
        MkCommandP (mkIdx 1) (Just "one") "cmd1"
          :<|| [ MkCommandP (mkIdx 2) (Just "two") "cmd2",
                 MkCommandP (mkIdx 3) Nothing "cmd3",
                 MkCommandP (mkIdx 4) Nothing "other"
               ]
  expected @=? result

noSplitNonKeyCmd :: TestTree
noSplitNonKeyCmd = testCase "Should not split non-key commands" $ do
  result <- translateCommandsSuccess legendMap ("echo ,," :<|| [])
  let expected = MkCommandP (mkIdx 1) Nothing "echo ,," :<|| []
  expected @=? result

cycleCmdFail :: TestTree
cycleCmdFail = testCase "Should fail on cycle" $ do
  result <- translateCommandsEx cyclicLegend ("a" :<|| [])
  MkCyclicKeyError "a -> b -> c -> a" @=? result

translateCommandsSuccess :: LegendMap -> NESeq Text -> IO (NESeq CommandP1)
translateCommandsSuccess map cmds =
  tryMySync (Legend.translateCommands map cmds Nothing) >>= \case
    Left ex -> assertFailure $ "Unexpected exception: " ++ displayException ex
    Right (x, _) -> pure x

translateCommandsSuccessEdges :: LegendMap -> NESeq Text -> EdgeArgs -> IO (Tuple2 (NESeq CommandP1) Edges)
translateCommandsSuccessEdges map cmds edges =
  tryMySync (Legend.translateCommands map cmds (Just edges)) >>= \case
    Left ex -> assertFailure $ "Unexpected exception: " ++ displayException ex
    Right (x, es) -> pure (x, es)

translateCommandsEx :: forall e. (Exception e) => LegendMap -> NESeq Text -> IO e
translateCommandsEx map cmds =
  try @_ @e (Legend.translateCommands map cmds Nothing) >>= \case
    Left ex -> pure ex
    Right x -> assertFailure $ "Unexpected success: " ++ show x

translateEdgesSpecs :: TestTree
translateEdgesSpecs =
  testGroup
    "With edges"
    [ testTranslateEdges1,
      testTranslateEdges2,
      testTranslateEdges3,
      testTranslateEdgesOneBoundsFailure,
      testTranslateEdgesBoundsFailure
    ]

testTranslateEdges1 :: TestTree
testTranslateEdges1 = testCase desc $ do
  (result, edges) <- translateCommandsSuccessEdges map cmds cliEdges
  expectedCmds @=? result
  expectedEdges @=? edges
  where
    desc = "Translates edges 1"

    cmds = ["cmd1", "some_aliases", "cmd2"]
    expectedCmds =
      [ MkCommandP (mkIdx 1) Nothing "cmd1",
        MkCommandP (mkIdx 2) Nothing "a1",
        MkCommandP (mkIdx 3) Nothing "a2",
        MkCommandP (mkIdx 4) Nothing "cmd2"
      ]

    cliEdges = [(mkIdx 1, mkIdx 3)]
    expectedEdges = [(mkIdx 1, mkIdx 4)]

    map =
      Map.fromList
        [ ("some_aliases", (["a1", "a2"], Nothing))
        ]

testTranslateEdges2 :: TestTree
testTranslateEdges2 = testCase desc $ do
  (result, edges) <- translateCommandsSuccessEdges map cmds cliEdges
  expectedCmds @=? result
  assertEdges expectedEdges edges
  where
    desc = "Translates edges 2"

    cmds = ["cmd1", "some_aliases", "cmd2"]
    expectedCmds =
      [ MkCommandP (mkIdx 1) Nothing "cmd1",
        MkCommandP (mkIdx 2) Nothing "a1",
        MkCommandP (mkIdx 3) Nothing "a2",
        MkCommandP (mkIdx 4) Nothing "cmd2"
      ]

    cliEdges = EdgeArgsSequential
    expectedEdges =
      mkEdges
        [ (1, 2),
          (1, 3),
          (2, 3),
          (2, 4),
          (3, 4)
        ]

    map =
      Map.fromList
        [ ("some_aliases", (["a1", "a2"], Just EdgeArgsSequential))
        ]

testTranslateEdges3 :: TestTree
testTranslateEdges3 = testCase desc $ do
  (result, edges) <- translateCommandsSuccessEdges map cmds cliEdges
  expectedCmds @=? result
  assertEdges expectedEdges edges
  where
    desc = "Translates edges 3"

    cmds = ["other1", "all", "other2"]
    expectedCmds =
      [ MkCommandP (mkIdx 1) Nothing "other1",
        MkCommandP (mkIdx 2) Nothing "cmd1",
        MkCommandP (mkIdx 3) Nothing "a1",
        MkCommandP (mkIdx 4) (Just "a2") "a22",
        MkCommandP (mkIdx 5) Nothing "a3",
        MkCommandP (mkIdx 6) (Just "cmd3") "cmd33",
        MkCommandP (mkIdx 7) Nothing "b1",
        MkCommandP (mkIdx 8) Nothing "b2",
        MkCommandP (mkIdx 9) Nothing "other2"
      ]

    cliEdges = mkEdgeArgs [(1, 2), (2, 3)]
    expectedEdges =
      mkEdges
        [ -- 'other1' has an edge to everything in 'all'
          (1, 2),
          (1, 3),
          (1, 4),
          (1, 5),
          (1, 6),
          (1, 7),
          (1, 8),
          -- Everything in 'all' has an edge to other2 (9)
          -- 'cmd1' has an edge to 'cmd3'
          (2, 6),
          (2, 9),
          -- Everything in 'aliases1' (3-5) has an edge to everything in
          -- 'aliases2' (7-8)
          (3, 7),
          (3, 8),
          (3, 9),
          (4, 5),
          (4, 7),
          (4, 8),
          (4, 9),
          (5, 7),
          (5, 8),
          (5, 9),
          -- Final 'other2' edges (9) and 'aliases2' internal edge (7-8).
          (6, 9),
          (7, 8),
          (7, 9),
          (8, 9)
        ]

    map =
      Map.fromList
        [ ( "all",
            ( ["cmd1", "aliases1", "cmd3", "aliases2"],
              mkMEdgeArgs [(1, 3), (2, 4)]
            )
          ),
          ("aliases1", (["a1", "a2", "a3"], mkMEdgeArgs [(2, 3)])),
          ("a2", (["a22"], Nothing)),
          ("cmd3", (["cmd33"], Nothing)),
          ("aliases2", (["b1", "b2"], mkMEdgeArgs [(1, 2)]))
        ]

testTranslateEdgesOneBoundsFailure :: TestTree
testTranslateEdgesOneBoundsFailure = testCase desc $ do
  ex <- translateCommandsEx @StringException map cmds
  "Index '2' in edge '1 -> 2' is out-of-bounds." @=? displayException ex
  where
    desc = "Single command with edges fails"

    cmds = ["cmd1", "some_aliases", "cmd2"]

    map =
      Map.fromList
        [ ("some_aliases", (["a1"], mkMEdgeArgs [(1, 2)]))
        ]

testTranslateEdgesBoundsFailure :: TestTree
testTranslateEdgesBoundsFailure = testCase desc $ do
  ex <- translateCommandsEx @StringException map cmds
  "Index '3' in edge '1 -> 3' is out-of-bounds." @=? displayException ex
  where
    desc = "Command with out-of-bounds edges fails"

    cmds = ["cmd1", "some_aliases", "cmd2"]

    map =
      Map.fromList
        [ ("some_aliases", (["a1", "a2"], mkMEdgeArgs [(1, 2), (1, 3)]))
        ]

mkMEdgeArgs :: List (Int, Int) -> Maybe EdgeArgs
mkMEdgeArgs = Just . mkEdgeArgs

mkEdgeArgs :: List (Int, Int) -> EdgeArgs
mkEdgeArgs = Exts.fromList . fmap (bimap mkIdx mkIdx)

mkEdges :: List (Int, Int) -> Edges
mkEdges = Exts.fromList . fmap (bimap mkIdx mkIdx)

assertEdges :: Edges -> Edges -> Assertion
assertEdges xs ys = assertList showEdge (Exts.toList xs) (Exts.toList ys)
  where
    showEdge (i, j) = show (unIdx i, unIdx j)
    unIdx = view (#unCommandIndex % #unPositive)

legendMap :: LegendMap
legendMap =
  Map.fromList
    $ over' _2 (,Nothing)
    <$> [ ("one", "cmd1" :<|| []),
          ("two", "cmd2" :<|| []),
          ("three", "cmd3" :<|| []),
          ("oneAndTwo", "one" :<|| ["two"]),
          ("all", "oneAndTwo" :<|| ["cmd3"])
        ]

cyclicLegend :: LegendMap
cyclicLegend =
  Map.fromList
    $ over' _2 (,Nothing)
    <$> [ ("a", "b" :<|| ["x"]),
          ("b", "c" :<|| ["x"]),
          ("c", "a" :<|| ["x"])
        ]

linesToMapSpecs :: TestTree
linesToMapSpecs =
  testGroup
    "linesToMap"
    [ parseMapAndSkip,
      duplicateKeysThrowErr
    ]

parseMapAndSkip :: TestTree
parseMapAndSkip = testCase "Should parse to map and skip comments" $ do
  result <-
    Legend.linesToMap
      [ unsafeKeyVal Nothing "a" ["b", "k"],
        unsafeKeyVal Nothing "b" ["c"]
      ]
  let expected =
        Map.fromList
          $ over' _2 (,Nothing)
          <$> [ ("a", "b" :<|| ["k"]),
                ("b", "c" :<|| [])
              ]
  expected @=? result

duplicateKeysThrowErr :: TestTree
duplicateKeysThrowErr = testCase "Duplicate keys should throw error" $ do
  try (Legend.linesToMap result) >>= \case
    Left (MkDuplicateKeyError s) -> "a" @=? s
    Right x -> assertFailure $ "Unexpected success: " ++ show x
  where
    result =
      [ unsafeKeyVal Nothing "a" ["b"],
        unsafeKeyVal Nothing "b" ["c"],
        unsafeKeyVal Nothing "a" ["d"]
      ]
