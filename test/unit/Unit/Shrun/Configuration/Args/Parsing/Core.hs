{-# LANGUAGE OverloadedLists #-}

module Unit.Shrun.Configuration.Args.Parsing.Core (tests) where

import Data.Text qualified as T
import Shrun.Configuration.Data.Graph
  ( CommandGraphArgs
      ( CommandGraphArgsEdges,
        CommandGraphArgsSequential
      ),
    Edges (MkEdges),
  )
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.CommandLogging qualified as CommandLogging
import Unit.Shrun.Configuration.Args.Parsing.CommonLogging qualified as CommonLogging
import Unit.Shrun.Configuration.Args.Parsing.ConsoleLogging qualified as ConsoleLogging
import Unit.Shrun.Configuration.Args.Parsing.FileLogging qualified as FileLogging
import Unit.Shrun.Configuration.Args.Parsing.Notify qualified as Notify
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing.Core"
    [ depTests,
      initTests,
      timeoutTests,
      CommonLogging.tests,
      CommandLogging.tests,
      ConsoleLogging.tests,
      FileLogging.tests,
      Notify.tests
    ]

depTests :: TestTree
depTests =
  testGroup
    "--command-graph"
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
  testPropertyNamed "Parses --command-graph" "testCommandGraph"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph (mkEdges [(1, 3), (2, 3), (1, 4)])
    depsStr = "1 -> 3, 2 -> 3, 1 -> 4"

testCommandGraphNoWs :: TestTree
testCommandGraphNoWs =
  testPropertyNamed "Parses --command-graph without whitespace" "testCommandGraphNoWs"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph (mkEdges [(1, 3), (2, 3), (1, 4)])
    depsStr = "1->3,2->3,1->4"

testCommandGraphExtendedEdge :: TestTree
testCommandGraphExtendedEdge =
  testPropertyNamed "Parses --command-graph extended-edges" "testCommandGraphExtendedEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
    depsStr = "1 -> 3 -> 2, 5 -> 6, 1 -> 4 -> 5"
    es = mkEdges [(1, 3), (3, 2), (5, 6), (1, 4), (4, 5)]

testCommandGraphMultiEdge :: TestTree
testCommandGraphMultiEdge =
  testPropertyNamed "Parses --command-graph multi-edges" "testCommandGraphMultiEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
    depsStr = "{1,2} -> 3, 3 -> {4,5}"
    es = mkEdges [(1, 3), (2, 3), (3, 4), (3, 5)]

testCommandGraphSetRange :: TestTree
testCommandGraphSetRange =
  testPropertyNamed "Parses --command-graph set range" "testCommandGraphSetRange"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
    depsStr = "1 -> {2, 4..6, 7..7}"
    es = mkEdges [(1, 2), (1, 4), (1, 5), (1, 6), (1, 7)]

testCommandGraphOneRangeEdge :: TestTree
testCommandGraphOneRangeEdge =
  testPropertyNamed "Parses --command-graph range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
    depsStr = "1..3"
    es = mkEdges [(1, 2), (2, 3)]

testCommandGraphRangeEdge :: TestTree
testCommandGraphRangeEdge =
  testPropertyNamed "Parses --command-graph range edges" "testCommandGraphRangeEdge"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
    depsStr = "1 -> 3 .. 6 -> 7..7"
    es = mkEdges [(1, 3), (3, 4), (4, 5), (5, 6), (6, 7)]

testCommandGraphComplex :: TestTree
testCommandGraphComplex =
  testPropertyNamed "Parses --command-graph complex" "testCommandGraphComplex"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", depsStr, "command"]
    expected = U.updateDefCoreArgs #commandGraph es
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

mkEdges :: List (Tuple2 Int Int) -> CommandGraphArgs
mkEdges =
  CommandGraphArgsEdges
    . MkEdges
    . fmap (bimap mkIdx mkIdx)

testCommandGraphSequential :: TestTree
testCommandGraphSequential =
  testPropertyNamed "Parses --command-graph sequential" "testCommandGraphSequential"
    $ U.verifyResult argList expected
  where
    argList = ["--command-graph", "sequential", "command"]
    expected = U.updateDefCoreArgs #commandGraph CommandGraphArgsSequential

testCommandGraphEmptyFail :: TestTree
testCommandGraphEmptyFail =
  testPropertyNamed "Parses empty --command-graph failure" "testCommandGraphEmptyFail"
    $ U.verifyFailureString argList "option --command-graph: Received empty input: ''"
  where
    argList = ["--command-graph=", "command"]

testCommandGraphNoSrcFail :: TestTree
testCommandGraphNoSrcFail =
  testPropertyNamed "Parses --command-graph -> 3" "testCommandGraphNoSrcFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", " -> 3", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:1:",
          "  |",
          "1 | -> 3",
          "  | ^",
          "Expected a set, arrow range, or index. Examples: '{1,2}', '1 .. 3', '1'."
        ]

testCommandGraphNoDestFail :: TestTree
testCommandGraphNoDestFail =
  testPropertyNamed "Parses --command-graph 3 ->" "testCommandGraphNoDestFail"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", " 3 -> ", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:6:",
          "  |",
          "1 | 3 -> ",
          "  |      ^",
          "Expected a set, arrow range, or index. Examples: '{1,2}', '1 .. 3', '1'."
        ]

testCommandGraphEmptySetFails :: TestTree
testCommandGraphEmptySetFails =
  testPropertyNamed "Parses empty --command-graph set failure" "testCommandGraphEmptySetFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", "1 -> {} -> 3", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:6:",
          "  |",
          "1 | 1 -> {} -> 3",
          "  |      ^",
          "Empty set"
        ]

testCommandGraphBadSetRangeFails :: TestTree
testCommandGraphBadSetRangeFails =
  testPropertyNamed "Parses --command-graph bad set range failure" "testCommandGraphBadSetRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", "1 -> {3..2}", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:6:",
          "  |",
          "1 | 1 -> {3..2}",
          "  |      ^",
          "Bad range. Expected 3 <= 2"
        ]

testCommandGraphBadArrowRangeFails :: TestTree
testCommandGraphBadArrowRangeFails =
  testPropertyNamed "Parses --command-graph bad arrow range failure" "testCommandGraphBadArrowRangeFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", "1 -> 3..2", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:6:",
          "  |",
          "1 | 1 -> 3..2",
          "  |      ^",
          "Bad range. Expected 3 <= 2"
        ]

testCommandGraphIndexFails :: TestTree
testCommandGraphIndexFails =
  testPropertyNamed "Parses --command-graph single index failure" "testCommandGraphIndexFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", "1", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:2:",
          "  |",
          "1 | 1",
          "  |  ^",
          "Empty edges"
        ]

testCommandGraphIndexSetFails :: TestTree
testCommandGraphIndexSetFails =
  testPropertyNamed "Parses --command-graph single index set failure" "testCommandGraphIndexSetFails"
    $ U.verifyFailureString argList expected
  where
    argList = ["--command-graph", "{1,2}", "command"]

    expected =
      T.unlines
        [ "option --command-graph: 1:6:",
          "  |",
          "1 | {1,2}",
          "  |      ^",
          "Empty edges"
        ]

timeoutTests :: TestTree
timeoutTests =
  testGroup
    "--timeout"
    [ testTimeoutShort,
      testTimeout,
      testTimeoutUnderscores,
      testTimeoutStringShort,
      testTimeoutString,
      testTimeoutWordFail,
      testTimeoutNegativeFail,
      testNoTimeout
    ]

testTimeoutShort :: TestTree
testTimeoutShort =
  testPropertyNamed "Parses -t" "testTimeoutShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t7", "command"]
    expected = U.updateDefCoreArgs #timeout 7

testTimeout :: TestTree
testTimeout =
  testPropertyNamed "Parses --timeout" "testTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=7", "command"]
    expected = U.updateDefCoreArgs #timeout 7

testTimeoutUnderscores :: TestTree
testTimeoutUnderscores =
  testPropertyNamed "Parses --timeout with underscores" "testTimeoutUnderscores"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1_000", "command"]
    expected = U.updateDefCoreArgs #timeout 1_000

testTimeoutStringShort :: TestTree
testTimeoutStringShort =
  testPropertyNamed "Parses -t 2h4s" "testTimeoutStringShort"
    $ U.verifyResult argList expected
  where
    argList = ["-t2h4s", "command"]
    expected = U.updateDefCoreArgs #timeout 7204

testTimeoutString :: TestTree
testTimeoutString =
  testPropertyNamed "Parses --timeout=1d2h3m4s" "testTimeoutString"
    $ U.verifyResult argList expected
  where
    argList = ["--timeout=1d2h3m4s", "command"]
    expected = U.updateDefCoreArgs #timeout 93784

testTimeoutWordFail :: TestTree
testTimeoutWordFail =
  testPropertyNamed "Parses --timeout=cat failure" "testTimeoutWordFail"
    $ U.verifyFailure argList
  where
    argList = ["--timeout=cat", "command"]

testTimeoutNegativeFail :: TestTree
testTimeoutNegativeFail =
  testPropertyNamed "Parses --timeout=cat -7" "testTimeoutNegativeFail"
    $ U.verifyFailure argList
  where
    argList = ["--timeout=-7", "command"]

testNoTimeout :: TestTree
testNoTimeout =
  testPropertyNamed "Parses --no-timeout" "testNoTimeout"
    $ U.verifyResult argList expected
  where
    argList = ["--no-timeout", "command"]
    expected = U.disableDefCoreArgs #timeout

initTests :: TestTree
initTests =
  testGroup
    "--init"
    [ testInitShort,
      testInit1,
      testInit2,
      parseNoInit
    ]

testInitShort :: TestTree
testInitShort =
  testPropertyNamed "Parses short init" "testInitShort"
    $ U.verifyResult argList expected
  where
    argList = ["-i. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

testInit1 :: TestTree
testInit1 =
  testPropertyNamed "Parses --init=. ~/.bashrc" "testInit1"
    $ U.verifyResult argList expected
  where
    argList = ["--init=. ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

testInit2 :: TestTree
testInit2 =
  testPropertyNamed "Parses --init \". ~/.bashrc\"" "testInit2"
    $ U.verifyResult argList expected
  where
    argList = ["--init", ". ~/.bashrc", "command"]
    expected = U.updateDefCoreArgs #init ". ~/.bashrc"

parseNoInit :: TestTree
parseNoInit =
  testPropertyNamed "Parses --no-init" "parseNoInit"
    $ U.verifyResult argList expected
  where
    argList = ["--no-init", "command"]
    expected = U.disableDefCoreArgs #init
