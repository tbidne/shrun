{-# LANGUAGE OverloadedLists #-}

module Unit.Shrun.Configuration.Args.Parsing.Graph (tests) where

import Data.Text qualified as T
import Shrun.Configuration.Data.Graph
  ( EdgeArgs
      ( EdgeArgsList,
        EdgeArgsSequential
      ),
    EdgeLabel (EdgeAnd),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.Graph"
    [ testCommandGraph,
      testCommandGraphNoWs,
      testCommandGraphExtendedEdge,
      testCommandGraphMultiEdge,
      testCommandGraphSetRange,
      testCommandGraphOneRangeEdge,
      testCommandGraphRangeEdge,
      testCommandGraphComplex,
      testCommandGraphSequential,
      testCommandGraphEmptyFail,
      testCommandGraphNoSrcFail,
      testCommandGraphNoDestFail,
      testCommandGraphEmptySetFails,
      testCommandGraphBadSetRangeFails,
      testCommandGraphBadArrowRangeFails,
      testCommandGraphIndexFails,
      testCommandGraphIndexSetFails
    ]

testCommandGraph :: TestTree
testCommandGraph =
  testPropertyNamed "Parses --edges" "testCommandGraph"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdges [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 -> 3, 2 -> 3, 1 -> 4"

testCommandGraphNoWs :: TestTree
testCommandGraphNoWs =
  testPropertyNamed "Parses --edges without whitespace" "testCommandGraphNoWs"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdges [(1, 3), (2, 3), (1, 4)])
    depsStr = "1->3,2->3,1->4"

testCommandGraphExtendedEdge :: TestTree
testCommandGraphExtendedEdge =
  testPropertyNamed "Parses --edges extended-edges" "testCommandGraphExtendedEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 -> 3 -> 2, 5 -> 6, 1 -> 4 -> 5"
    es = mkEdges [(1, 3), (3, 2), (5, 6), (1, 4), (4, 5)]

testCommandGraphMultiEdge :: TestTree
testCommandGraphMultiEdge =
  testPropertyNamed "Parses --edges multi-edges" "testCommandGraphMultiEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "{1,2} -> 3, 3 -> {4,5}"
    es = mkEdges [(1, 3), (2, 3), (3, 4), (3, 5)]

testCommandGraphSetRange :: TestTree
testCommandGraphSetRange =
  testPropertyNamed "Parses --edges set range" "testCommandGraphSetRange"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 -> {2, 4..6, 7..7}"
    es = mkEdges [(1, 2), (1, 4), (1, 5), (1, 6), (1, 7)]

testCommandGraphOneRangeEdge :: TestTree
testCommandGraphOneRangeEdge =
  testPropertyNamed "Parses --edges range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1..3"
    es = mkEdges [(1, 2), (2, 3)]

testCommandGraphRangeEdge :: TestTree
testCommandGraphRangeEdge =
  testPropertyNamed "Parses --edges range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 -> 3 .. 6 -> 7..7"
    es = mkEdges [(1, 3), (3, 4), (4, 5), (5, 6), (6, 7)]

testCommandGraphComplex :: TestTree
testCommandGraphComplex =
  testPropertyNamed "Parses --edges complex" "testCommandGraphComplex"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "{1,2} -> {3..5,6} -> 7 .. 9 -> {10, 11}"
    es =
      mkEdges
        [ (1, 3),
          (1, 4),
          (1, 5),
          (1, 6),
          (2, 3),
          (2, 4),
          (2, 5),
          (2, 6),
          (3, 7),
          (4, 7),
          (5, 7),
          (6, 7),
          (7, 8),
          (8, 9),
          (9, 10),
          (9, 11)
        ]

mkEdges :: Seq (Tuple2 Int Int) -> WithDisabled EdgeArgs
mkEdges =
  With
    . EdgeArgsList
    . MkEdges
    . fmap (\(s, d) -> (mkIdx s, mkIdx d, EdgeAnd))

testCommandGraphSequential :: TestTree
testCommandGraphSequential =
  testPropertyNamed "Parses --edges sequential" "testCommandGraphSequential"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", "sequential", "command"]
    expected = U.updateDefArgs #edges (With EdgeArgsSequential)

testCommandGraphEmptyFail :: TestTree
testCommandGraphEmptyFail =
  testPropertyNamed "Parses empty --edges failure" "testCommandGraphEmptyFail"
    $ U.verifyFailureString argList "option --edges: Received empty input: ''"
  where
    argList = ["--edges=", "command"]

testCommandGraphNoSrcFail :: TestTree
testCommandGraphNoSrcFail =
  testPropertyNamed "Parses --edges -> 3" "testCommandGraphNoSrcFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " -> 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:1:",
          "  |",
          "1 | -> 3",
          "  | ^",
          "Expected a set, arrow range, or index. Examples: '{1,2}', '1 .. 3', '1'."
        ]

testCommandGraphNoDestFail :: TestTree
testCommandGraphNoDestFail =
  testPropertyNamed "Parses --edges 3 ->" "testCommandGraphNoDestFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " 3 -> ", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 3 -> ",
          "  |      ^",
          "Expected a set, arrow range, or index. Examples: '{1,2}', '1 .. 3', '1'."
        ]

testCommandGraphEmptySetFails :: TestTree
testCommandGraphEmptySetFails =
  testPropertyNamed "Parses empty --edges set failure" "testCommandGraphEmptySetFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 -> {} -> 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 -> {} -> 3",
          "  |      ^",
          "Empty set"
        ]

testCommandGraphBadSetRangeFails :: TestTree
testCommandGraphBadSetRangeFails =
  testPropertyNamed "Parses --edges bad set range failure" "testCommandGraphBadSetRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 -> {3..2}", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 -> {3..2}",
          "  |      ^",
          "Bad range. Expected 3 <= 2"
        ]

testCommandGraphBadArrowRangeFails :: TestTree
testCommandGraphBadArrowRangeFails =
  testPropertyNamed "Parses --edges bad arrow range failure" "testCommandGraphBadArrowRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 -> 3..2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 -> 3..2",
          "  |      ^",
          "Bad range. Expected 3 <= 2"
        ]

testCommandGraphIndexFails :: TestTree
testCommandGraphIndexFails =
  testPropertyNamed "Parses --edges single index failure" "testCommandGraphIndexFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:2:",
          "  |",
          "1 | 1",
          "  |  ^",
          "Empty edges"
        ]

testCommandGraphIndexSetFails :: TestTree
testCommandGraphIndexSetFails =
  testPropertyNamed "Parses --edges single index set failure" "testCommandGraphIndexSetFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "{1,2}", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | {1,2}",
          "  |      ^",
          "Empty edges"
        ]
