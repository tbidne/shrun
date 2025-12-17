{-# LANGUAGE OverloadedLists #-}

module Unit.Shrun.Configuration.Args.Parsing.Graph (tests) where

import Data.Text qualified as T
import Shrun.Configuration.Data.Graph
  ( EdgeArgs
      ( EdgeArgsList,
        EdgeArgsSequential
      ),
    EdgeLabel (EdgeAnd, EdgeAny, EdgeOr),
    EdgeSequential (EdgeSequentialAnd, EdgeSequentialAny, EdgeSequentialOr),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled (With))
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.Graph"
    [ successTests,
      failureTests
    ]

successTests :: TestTree
successTests =
  testGroup
    "Success"
    [ testCommandGraph,
      testCommandGraphAnd,
      testCommandGraphOr,
      testCommandGraphAny,
      testCommandGraphNoWs,
      testCommandGraphWs,
      testCommandGraphExtendedEdge,
      testCommandGraphMultiEdge,
      testCommandGraphSetRange,
      testCommandGraphOneRangeEdge,
      testCommandGraphOneRangeEdgeAnd,
      testCommandGraphOneRangeEdgeOr,
      testCommandGraphOneRangeEdgeAny,
      testCommandGraphRangeEdge,
      testCommandGraphComplex,
      testCommandGraphSeqAnd,
      testCommandGraphSeqOr,
      testCommandGraphSeqAny
    ]

testCommandGraph :: TestTree
testCommandGraph =
  testPropertyNamed "Parses --edges" "testCommandGraph"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesAnd [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 & 3, 2 & 3, 1 & 4"

testCommandGraphAnd :: TestTree
testCommandGraphAnd =
  testPropertyNamed "Parses 'and' --edges with alternative syntax" "testCommandGraphAnd"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesAnd [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 & 3, 2 & 3, 1 & 4"

testCommandGraphOr :: TestTree
testCommandGraphOr =
  testPropertyNamed "Parses 'or' --edges" "testCommandGraphOr"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesOr [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 | 3, 2 | 3, 1 | 4"

testCommandGraphAny :: TestTree
testCommandGraphAny =
  testPropertyNamed "Parses 'any' --edges" "testCommandGraphAny"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesAny [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 ; 3, 2 ; 3, 1 ; 4"

testCommandGraphNoWs :: TestTree
testCommandGraphNoWs =
  testPropertyNamed "Parses --edges without whitespace" "testCommandGraphNoWs"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesAnd [(1, 3), (2, 3), (1, 4)])
    depsStr = "1&3,2&3,1&4"

testCommandGraphWs :: TestTree
testCommandGraphWs =
  testPropertyNamed "Parses --edges with copious whitespace" "testCommandGraphWs"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges (mkEdgesAnd es)
    depsStr = "{ 1 , 2 .. 3 } & 4 , 2 & .. 3"

    es =
      [ (1, 4),
        (2, 4),
        (3, 4),
        (2, 3)
      ]

testCommandGraphExtendedEdge :: TestTree
testCommandGraphExtendedEdge =
  testPropertyNamed "Parses --edges extended-edges" "testCommandGraphExtendedEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 & 3 & 2, 5 & 6, 1 & 4 & 5"
    es = mkEdgesAnd [(1, 3), (3, 2), (5, 6), (1, 4), (4, 5)]

testCommandGraphMultiEdge :: TestTree
testCommandGraphMultiEdge =
  testPropertyNamed "Parses --edges multi-edges" "testCommandGraphMultiEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "{1,2} & 3, 3 & {4,5}"
    es = mkEdgesAnd [(1, 3), (2, 3), (3, 4), (3, 5)]

testCommandGraphSetRange :: TestTree
testCommandGraphSetRange =
  testPropertyNamed "Parses --edges set range" "testCommandGraphSetRange"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 & {2, 4..6, 7..8}"
    es = mkEdgesAnd [(1, 2), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8)]

testCommandGraphOneRangeEdge :: TestTree
testCommandGraphOneRangeEdge =
  testPropertyNamed "Parses --edges range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1&..3"
    es = mkEdgesAnd [(1, 2), (2, 3)]

testCommandGraphOneRangeEdgeAnd :: TestTree
testCommandGraphOneRangeEdgeAnd =
  testPropertyNamed "Parses 'and' --edges range edges" "testCommandGraphOneRangeEdgeAnd"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 &.. 3"
    es = mkEdgesAnd [(1, 2), (2, 3)]

testCommandGraphOneRangeEdgeOr :: TestTree
testCommandGraphOneRangeEdgeOr =
  testPropertyNamed "Parses 'or' --edges range edges" "testCommandGraphOneRangeEdgeOr"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 |.. 3"
    es = mkEdgesOr [(1, 2), (2, 3)]

testCommandGraphOneRangeEdgeAny :: TestTree
testCommandGraphOneRangeEdgeAny =
  testPropertyNamed "Parses 'any' --edges range edges" "testCommandGraphOneRangeEdgeAny"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 ;.. 3"
    es = mkEdgesAny [(1, 2), (2, 3)]

testCommandGraphRangeEdge :: TestTree
testCommandGraphRangeEdge =
  testPropertyNamed "Parses --edges range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "1 & 3 &.. 6 & 7&..8"
    es = mkEdgesAnd [(1, 3), (3, 4), (4, 5), (5, 6), (6, 7), (7, 8)]

testCommandGraphComplex :: TestTree
testCommandGraphComplex =
  testPropertyNamed "Parses --edges complex" "testCommandGraphComplex"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", depsStr, "command"]
    expected = U.updateDefArgs #edges es
    depsStr = "{1,2} & {3..5,6} ; 7 &.. 9 | {10, 11}"
    es =
      mkEdges
        [ (1, 3, EdgeAnd),
          (1, 4, EdgeAnd),
          (1, 5, EdgeAnd),
          (1, 6, EdgeAnd),
          (2, 3, EdgeAnd),
          (2, 4, EdgeAnd),
          (2, 5, EdgeAnd),
          (2, 6, EdgeAnd),
          (3, 7, EdgeAny),
          (4, 7, EdgeAny),
          (5, 7, EdgeAny),
          (6, 7, EdgeAny),
          (7, 8, EdgeAnd),
          (8, 9, EdgeAnd),
          (9, 10, EdgeOr),
          (9, 11, EdgeOr)
        ]

mkEdgesAny :: Seq (Tuple2 Int Int) -> WithDisabled EdgeArgs
mkEdgesAny = mkEdgesLbl EdgeAny

mkEdgesAnd :: Seq (Tuple2 Int Int) -> WithDisabled EdgeArgs
mkEdgesAnd = mkEdgesLbl EdgeAnd

mkEdgesOr :: Seq (Tuple2 Int Int) -> WithDisabled EdgeArgs
mkEdgesOr = mkEdgesLbl EdgeOr

mkEdgesLbl :: EdgeLabel -> Seq (Tuple2 Int Int) -> WithDisabled EdgeArgs
mkEdgesLbl lbl = mkEdges . fmap (\(s, d) -> (s, d, lbl))

mkEdges :: Seq (Tuple3 Int Int EdgeLabel) -> WithDisabled EdgeArgs
mkEdges =
  With
    . EdgeArgsList
    . MkEdges
    . fmap (\(s, d, lbl) -> (mkIdx s, mkIdx d, lbl))

testCommandGraphSeqAnd :: TestTree
testCommandGraphSeqAnd =
  testPropertyNamed "Parses --edges &&&" "testCommandGraphSeqAnd"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", "&&&", "command"]
    expected = U.updateDefArgs #edges (With (EdgeArgsSequential EdgeSequentialAnd))

testCommandGraphSeqOr :: TestTree
testCommandGraphSeqOr =
  testPropertyNamed "Parses --edges |||" "testCommandGraphSeqOr"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", "|||", "command"]
    expected = U.updateDefArgs #edges (With (EdgeArgsSequential EdgeSequentialOr))

testCommandGraphSeqAny :: TestTree
testCommandGraphSeqAny =
  testPropertyNamed "Parses --edges ;;;" "testCommandGraphSeqAny"
    $ U.verifyResult argList expected
  where
    argList = ["--edges", ";;;", "command"]
    expected = U.updateDefArgs #edges (With (EdgeArgsSequential EdgeSequentialAny))

failureTests :: TestTree
failureTests =
  testGroup
    "Failure"
    [ testCommandGraphEmptyFail,
      testCommandGraphNoSrcFail,
      testCommandGraphNoDestFail,
      testCommandGraphNoDestDotsFail,
      testCommandGraphGeneralFail,
      testCommandGraphGeneralFail2,
      testCommandGraphGeneralFail3,
      testCommandGraphEmptySetFails,
      testCommandGraphEmptySetFails2,
      testCommandGraphBadSetRangeFails,
      testCommandGraphBadSetRangeFails2,
      testCommandGraphBadArrowRangeFails,
      testCommandGraphBadArrowRangeFails2,
      testCommandGraphBadArrowRangeFails3,
      testCommandGraphIndexFails,
      testCommandGraphIndexSetFails,
      testBadArrowFails,
      testBadArrowFails2,
      testSetDotsErr,
      testDotsSetErr
    ]

testCommandGraphEmptyFail :: TestTree
testCommandGraphEmptyFail =
  testPropertyNamed "Parses empty --edges failure" "testCommandGraphEmptyFail"
    $ U.verifyFailureString argList "option --edges: Received empty input: ''"
  where
    argList = ["--edges=", "command"]

testCommandGraphNoSrcFail :: TestTree
testCommandGraphNoSrcFail =
  testPropertyNamed "Parses --edges & 3" "testCommandGraphNoSrcFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " & 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:1:",
          "  |",
          "1 | & 3",
          "  | ^",
          "unexpected edge",
          "expecting a vertex (e.g. '3', '{1,5}')"
        ]

testCommandGraphNoDestFail :: TestTree
testCommandGraphNoDestFail =
  testPropertyNamed "Parses --edges 3 &" "testCommandGraphNoDestFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " 3 & ", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:5:",
          "  |",
          "1 | 3 & ",
          "  |     ^",
          "unexpected end of input",
          "expecting a vertex (e.g. '3', '{1,5}'), range ('..'), or white space"
        ]

testCommandGraphNoDestDotsFail :: TestTree
testCommandGraphNoDestDotsFail =
  testPropertyNamed "Parses --edges 3 &.." "testCommandGraphNoDestDotsFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " 3 &.. ", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:7:",
          "  |",
          "1 | 3 &.. ",
          "  |       ^",
          "unexpected end of input",
          "expecting a single vertex (e.g. '3') or white space"
        ]

testCommandGraphGeneralFail :: TestTree
testCommandGraphGeneralFail =
  testPropertyNamed "Parses --edges && 3" "testCommandGraphGeneralFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " && 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:1:",
          "  |",
          "1 | && 3",
          "  | ^^^",
          "unexpected \"&& \"",
          "expecting comma-delimited edge(s) (e.g. \"1 & 2, {3,4} ; 1, 4 &.. 6\") or sequential literals (\"&&&\", \"|||\", \";;;\")"
        ]

testCommandGraphGeneralFail2 :: TestTree
testCommandGraphGeneralFail2 =
  testPropertyNamed "Parses --edges &&" "testCommandGraphGeneralFail2"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " &&", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:1:",
          "  |",
          "1 | &&",
          "  | ^^",
          "unexpected \"&&\"",
          "expecting comma-delimited edge(s) (e.g. \"1 & 2, {3,4} ; 1, 4 &.. 6\") or sequential literals (\"&&&\", \"|||\", \";;;\")"
        ]

testCommandGraphGeneralFail3 :: TestTree
testCommandGraphGeneralFail3 =
  testPropertyNamed "Parses --edges &" "testCommandGraphGeneralFail3"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", " &", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:1:",
          "  |",
          "1 | &",
          "  | ^",
          "unexpected '&'",
          "expecting comma-delimited edge(s) (e.g. \"1 & 2, {3,4} ; 1, 4 &.. 6\") or sequential literals (\"&&&\", \"|||\", \";;;\")"
        ]

testCommandGraphEmptySetFails :: TestTree
testCommandGraphEmptySetFails =
  testPropertyNamed "Parses empty --edges set failure" "testCommandGraphEmptySetFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & {} & 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 & {} & 3",
          "  |      ^",
          "Empty set"
        ]

testCommandGraphEmptySetFails2 :: TestTree
testCommandGraphEmptySetFails2 =
  testPropertyNamed "Parses empty --edges set failure 2" "testCommandGraphEmptySetFails2"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & { } & 3", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:7:",
          "  |",
          "1 | 1 & { } & 3",
          "  |       ^",
          "Empty set"
        ]

testCommandGraphBadSetRangeFails :: TestTree
testCommandGraphBadSetRangeFails =
  testPropertyNamed "Parses --edges bad set range failure" "testCommandGraphBadSetRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & {3..2}", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 & {3..2}",
          "  |      ^",
          "Bad range. Expected 3 < 2"
        ]

testCommandGraphBadSetRangeFails2 :: TestTree
testCommandGraphBadSetRangeFails2 =
  testPropertyNamed "Parses --edges bad set range failure 2" "testCommandGraphBadSetRangeFails2"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & {2..2}", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 & {2..2}",
          "  |      ^",
          "Bad range. Expected 2 < 2"
        ]

testCommandGraphBadArrowRangeFails :: TestTree
testCommandGraphBadArrowRangeFails =
  testPropertyNamed "Parses --edges bad edge range failure" "testCommandGraphBadArrowRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & 3&..2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:10:",
          "  |",
          "1 | 1 & 3&..2",
          "  |          ^",
          "Bad range. Expected 3 < 2"
        ]

testCommandGraphBadArrowRangeFails2 :: TestTree
testCommandGraphBadArrowRangeFails2 =
  testPropertyNamed "Parses --edges bad edge range failure 2" "testCommandGraphBadArrowRangeFails2"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & 3..2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | 1 & 3..2",
          "  |      ^",
          "Expected an edge, found '..'. Perhaps you wanted an edge range e.g. '&..'?"
        ]

testCommandGraphBadArrowRangeFails3 :: TestTree
testCommandGraphBadArrowRangeFails3 =
  testPropertyNamed "Parses --edges bad edge range failure 3" "testCommandGraphBadArrowRangeFails3"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 & 2&..2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:10:",
          "  |",
          "1 | 1 & 2&..2",
          "  |          ^",
          "Bad range. Expected 2 < 2"
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
          "unexpected end of input",
          "expecting an edge ('&', '|', ';'), digit, or white space"
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
          "unexpected end of input",
          "expecting an edge ('&', '|', ';') or white space"
        ]

testBadArrowFails :: TestTree
testBadArrowFails =
  testPropertyNamed "Parses --edges bad arrow failure" "testBadArrowFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 -> 2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:3:",
          "  |",
          "1 | 1 -> 2",
          "  |   ^",
          "unexpected '-'",
          "expecting an edge ('&', '|', ';') or white space"
        ]

testBadArrowFails2 :: TestTree
testBadArrowFails2 =
  testPropertyNamed "Parses --edges bad arrow failure 2" "testBadArrowFails2"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 % 2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:3:",
          "  |",
          "1 | 1 % 2",
          "  |   ^",
          "unexpected '%'",
          "expecting an edge ('&', '|', ';') or white space"
        ]

testSetDotsErr :: TestTree
testSetDotsErr =
  testPropertyNamed "Parses --edges bad set dots failure" "testSetDotsErr"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "{1} &.. 2", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:6:",
          "  |",
          "1 | {1} &.. 2",
          "  |      ^",
          "Edge ranges (e.g. '&..') are not allowed with set syntax."
        ]

testDotsSetErr :: TestTree
testDotsSetErr =
  testPropertyNamed "Parses --edges bad dots set failure" "testDotsSetErr"
    $ U.verifyFailureString argList expected
  where
    argList = ["--edges", "1 &.. {2}", "command"]

    expected =
      T.unlines
        [ "option --edges: 1:7:",
          "  |",
          "1 | 1 &.. {2}",
          "  |       ^",
          "Edge ranges (e.g. '&..') are not allowed with set syntax."
        ]
