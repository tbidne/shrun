{-# LANGUAGE OverloadedLists #-}

module Unit.Shrun.Configuration.Data.Graph (tests) where

import Data.Graph qualified as Graph
import Data.List qualified as L
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Command.Types
  ( CommandP (MkCommandP),
    CommandP1,
    fromVertex,
    toVertex,
  )
import Shrun.Configuration.Data.Graph
  ( EdgeArgs (EdgeArgsList, EdgeArgsSequential),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.Graph qualified as CDG
import Shrun.Configuration.Default (Default (def))
import Unit.Prelude
import Unit.Shrun.Logging.Generators qualified as Logging.G

-- - fromV vertex always == returned tuple index

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Data.Graph"
    [ testDuplicateEdgesFails,
      testNonExtantEdgeSrc,
      testNonExtantEdgeDest,
      testNoRootsFails,
      testUnreachableNodeFails,
      testCycleFails,
      testTrivialCycleFails,
      testTrivialGraphProp,
      testVertexCommandIndexRel,
      testSequential
    ]

testDuplicateEdgesFails :: TestTree
testDuplicateEdgesFails = testCase "Duplicate edges fails" $ do
  runFailure expected es cmds
  where
    expected = "Found duplicates: (1,2) (1,3)"
    es = [(1, 2), (1, 3), (1, 2), (2, 3), (1, 3)]
    cmds = unsafeListToNESeq $ mkCmds [1]

testNonExtantEdgeSrc :: TestTree
testNonExtantEdgeSrc = testCase "Missing edge src fails" $ do
  runFailure expected es cmds
  where
    expected = "Command index 4 in dependency 1 -> 4 does not exist."
    es = [(1, 2), (1, 4)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 3]

testNonExtantEdgeDest :: TestTree
testNonExtantEdgeDest = testCase "Missing edge dest fails" $ do
  runFailure expected es cmds
  where
    expected = "Command index 4 in dependency 4 -> 1 does not exist."
    es = [(1, 2), (4, 1)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 3]

testNoRootsFails :: TestTree
testNoRootsFails = testCase "No roots fails" $ do
  runFailure expected es cmds
  where
    expected = "No root command(s) found! There is probably a cycle."
    es = [(1, 2), (2, 3), (3, 4), (4, 1)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 4]

testUnreachableNodeFails :: TestTree
testUnreachableNodeFails = testCase "Unreachable node fails" $ do
  runFailure expected es cmds
  where
    expected = "The following commands are not reachable: 3, 4. There is probably a cycle."
    es = [(1, 2), (3, 4), (4, 3)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 4]

testCycleFails :: TestTree
testCycleFails = testCase "Cycle fails" $ do
  runFailure expected es cmds
  where
    expected = "Found command cycle: 1 -> 2 -> 3 -> 2"
    es = [(1, 2), (2, 3), (3, 2)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 3]

testTrivialCycleFails :: TestTree
testTrivialCycleFails = testCase "Trivial cycle fails" $ do
  runFailure expected es cmds
  where
    expected = "Found command cycle: 1 -> 2 -> 2"
    es = [(1, 2), (2, 2)]
    cmds = unsafeListToNESeq $ mkCmds [1 .. 2]

testTrivialGraphProp :: TestTree
testTrivialGraphProp = testProp desc "testTrivialGraphProp" $ do
  cmds <- forAll genCmds
  let t = CDG.mkTrivialGraph cmds
  g <- CDG.mkGraph def cmds

  let ggraph = g ^. #graph
      tgraph = t ^. #graph

      gfromV = g ^. #fromV
      tfromV = t ^. #fromV

      gvertices = Graph.vertices ggraph
      tvertices = Graph.vertices tgraph

  -- CDG equality is based on #graph, so we manually compare some we can
  -- compare roots too.
  ggraph === tgraph
  g ^. #roots === t ^. #roots

  -- Probably unnecessary, but it doesn't slow down the tests, so whatever.
  for_ gvertices $ \gv -> gfromV gv === tfromV gv
  for_ tvertices $ \tv -> gfromV tv === tfromV tv
  where
    desc = "mkGraph [] === mkTrivialGraph"

testVertexCommandIndexRel :: TestTree
testVertexCommandIndexRel = testProp desc "testVertexCommandIndexRel" $ do
  cmds <- forAll genCmds
  cdg <- CDG.mkGraph def cmds

  let fromV = cdg ^. #fromV
      vertices = Graph.vertices $ cdg ^. #graph

  for_ vertices $ \v -> do
    let (cmd, cmdIdx, _) = fromV v

    -- Command should have its own index.
    cmd ^. #index === cmdIdx

    -- Conversion functions should provide isomorphism.
    v === toVertex cmdIdx
    fromVertex v === cmdIdx
  where
    desc = "Verify Vertex <-> CommandIndex conversions"

testSequential :: TestTree
testSequential = testProp desc "testSequential" $ do
  commands <- forAll genCmds
  -- Try/catch because exceptions screw with hedgehog's nice output.
  cdg <-
    tryMySync (CDG.mkGraph EdgeArgsSequential commands) >>= \case
      Right cg -> pure cg
      Left ex -> do
        annotate $ "Exception creating graph: " ++ displayException ex
        failure

  annotateShow cdg

  let fromV = cdg ^. #fromV
      idxToV = cdg ^. #idxToV

      go :: Vertex -> List CommandP1 -> PropertyT IO ()
      go v [] = do
        annotate $ "Found vertex but no commands: " ++ show v
        failure
      go v (MkCommandP cmdIdx _ _ : cmds) = do
        -- Sanity checks
        -- 1. This vertex corresponds exactly to the command index
        --    (modulo 1, for the offset).
        v === cmdIdx ^. (#unCommandIndex % #unPositive) - 1
        -- 2. Same as above, just testing our functions.
        v === toVertex cmdIdx
        fromVertex v === cmdIdx

        let (_, _, edges) = fromV v
        case (edges, cmds) of
          -- 1. No more edges or commands: Good.
          ([], []) -> pure ()
          -- 2. Non-empty edges but no commands: Bad.
          (es@(_ : _), []) -> do
            annotate $ "Empty command but non-empty edges: " ++ show es
            failure
          -- 3. Empty edges but non-empty commands: Bad.
          ([], cs@(_ : _)) -> do
            annotate $ "Empty edges but non-empty commands: " ++ show cs
            failure
          -- 4. More than one edge: Bad.
          (es@(_ : _ : _), _) -> do
            annotate $ "More than one edge: " ++ show es
            failure
          -- 5. Exactly one edge and some commands: Fine, recurse.
          ([edgeIdx], _) -> do
            case idxToV edgeIdx of
              Nothing -> do
                annotate $ "Failed converting edgeIdx: " ++ show edgeIdx
                failure
              Just edgeV -> go edgeV cmds

  case toList (cdg ^. #roots) of
    [] -> do
      annotate "Expected exactly one root, received none"
      failure
    [r] -> go r (toList commands)
    xs -> do
      annotate $ "Expected exactly one root, received more: " ++ show xs
      failure
  where
    desc = "Creates sequenced indexes"

runFailure :: String -> Seq (Int, Int) -> NESeq CommandP1 -> IO ()
runFailure expected edgeInts cmds =
  -- Specifically catching the TextException to avoid callstacks for GHC 9.10.
  -- We may want to replace this with a specific exception at some point.
  try (CDG.mkGraph (EdgeArgsList edges) cmds) >>= \case
    Right g -> assertFailure $ "Expected failure, received success: " ++ show g
    Left (MkStringException str) -> expected @=? str
  where
    edges = MkEdges $ bimap mkIdx mkIdx <$> edgeInts

genCmds :: Gen (NESeq CommandP1)
genCmds =
  -- Re-index commands in order, since randomly generated ones will die during
  -- graph creation.
  unsafeListToNESeq
    . L.zipWith updateIdx [1 ..]
    <$> g
  where
    g = G.list (R.linearFrom 1 1 20) Logging.G.genCommand

    updateIdx :: Int -> CommandP1 -> CommandP1
    updateIdx i (MkCommandP _ a1 a2) = MkCommandP (mkIdx i) a1 a2

mkCmds :: [Int] -> [CommandP1]
mkCmds = fmap (\i -> MkCommandP (mkIdx i) Nothing ("cmd" <> showt i))
