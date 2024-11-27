{-# LANGUAGE QuasiQuotes #-}

module Functional.ReadStrategyTest
  ( ReadStrategyOpt (..),
    ReadStrategyTestParams (..),
    multiTestReadStrategy,
    testReadStrategy,
  )
where

import Data.Tagged (Tagged (Tagged))
import Options.Applicative qualified as OA
import Shrun.Prelude
import Shrun.Utils qualified as Utils
import Test.Tasty (TestName, TestTree, askOption, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Options
  ( IsOption
      ( defaultValue,
        optionCLParser,
        optionHelp,
        optionName,
        parseValue
      ),
    mkOptionCLParser,
  )

-- | Test parameters, used for running tests against multiple read
-- strategies.
data ReadStrategyTestParams where
  -- | Simple test that should be parametric over ReadStrategy.
  ReadStrategyTestParametricSimple ::
    -- | Test description
    String ->
    -- | Test runner
    (List String -> IO a) ->
    -- | Args
    (List String) ->
    -- | Assertions
    (a -> IO ()) ->
    ReadStrategyTestParams
  -- | Simple test, not parametric.
  ReadStrategyTestSimple ::
    -- | Test description
    String ->
    -- | Test runner
    (List String -> IO a) ->
    -- | Args
    (List String) ->
    -- | Block Assertions
    (a -> IO ()) ->
    -- | BlockLineBuffer assertions
    (a -> IO ()) ->
    ReadStrategyTestParams
  -- | Test with more complicated setup that should be parametric over
  -- ReadStrategy.
  ReadStrategyTestParametricSetup ::
    -- | Test description
    String ->
    -- | Test runner
    (List String -> IO a) ->
    -- | Args setup. The argument is an OsPath verison of the read-startegy
    -- type i.e. block, buffer, or default. We use this for creating unique
    -- paths i.e. we do not want two read-strategy tests to use the same
    -- path when creating log files.
    (OsPath -> IO (List String, r)) ->
    -- | Assertions
    ((a, r) -> IO ()) ->
    ReadStrategyTestParams
  -- | Test with more complicated setup.
  ReadStrategyTestSetup ::
    -- | Test description
    String ->
    -- | Test runner
    (List String -> IO a) ->
    -- | Args setup.  The argument is an OsPath verison of the read-startegy
    -- type i.e. block, buffer, or default. We use this for creating unique
    -- paths i.e. we do not want two read-strategy tests to use the same
    -- path when creating log files.
    (OsPath -> IO (List String, r)) ->
    -- | Block Assertions
    ((a, r) -> IO ()) ->
    -- | BlockLineBuffer assertions
    ((a, r) -> IO ()) ->
    ReadStrategyTestParams

-- | Runs multiple tests against one or more command-log read strategies.
multiTestReadStrategy :: List ReadStrategyTestParams -> List TestTree
multiTestReadStrategy xs = xs >>= testReadStrategy

-- | Runs a test against one or more command-log read strategies.
testReadStrategy :: ReadStrategyTestParams -> List TestTree
testReadStrategy (ReadStrategyTestParametricSimple desc runner args assertResults) =
  [ askOption $ \case
      ReadStrategyBlock -> blockTest
      ReadStrategyBlockLineBuffer -> bufferTest
      ReadStrategyAll ->
        testGroup
          "All command-log read strategies"
          [ defaultTest,
            blockTest,
            bufferTest
          ]
  ]
  where
    defaultTest = defaultTestSimple desc runner args assertResults
    blockTest = blockTestSimple desc runner args assertResults
    bufferTest = bufferTestSimple desc runner args assertResults
testReadStrategy (ReadStrategyTestSimple desc runner args blockAssertions blockLineBufferAssertions) =
  [ askOption $ \case
      ReadStrategyBlock -> blockTest
      ReadStrategyBlockLineBuffer -> bufferTest
      ReadStrategyAll ->
        testGroup
          "All command-log read strategies"
          [ blockTest,
            bufferTest
          ]
  ]
  where
    blockTest = blockTestSimple desc runner args blockAssertions
    bufferTest = bufferTestSimple desc runner args blockLineBufferAssertions
testReadStrategy (ReadStrategyTestParametricSetup desc runner mkArgs assertResults) =
  [ askOption $ \case
      ReadStrategyBlock -> blockTest
      ReadStrategyBlockLineBuffer -> bufferTest
      ReadStrategyAll ->
        testGroup
          "All command-log read strategies"
          [ defaultTest,
            blockTest,
            bufferTest
          ]
  ]
  where
    defaultTest = defaultTestSetup desc runner (mkArgs [osp|default|]) assertResults
    blockTest = blockTestSetup desc runner (mkArgs [osp|block|]) assertResults
    bufferTest = bufferTestSetup desc runner (mkArgs [osp|buffer|]) assertResults
testReadStrategy (ReadStrategyTestSetup desc runner mkArgs blockAssertions blockLineBufferAssertions) =
  [ askOption $ \case
      ReadStrategyBlock -> blockTest
      ReadStrategyBlockLineBuffer -> bufferTest
      ReadStrategyAll ->
        testGroup
          "All command-log read strategies"
          [ blockTest,
            bufferTest
          ]
  ]
  where
    blockTest = blockTestSetup desc runner (mkArgs [osp|block|]) blockAssertions
    bufferTest = bufferTestSetup desc runner (mkArgs [osp|buffer|]) blockLineBufferAssertions

-- NOTE: For parametric tests, we also want to verify the default
-- (i.e. no args) behavior passes, since that is how such examples are defined.

defaultTestSimple ::
  String ->
  (List String -> IO t) ->
  List String ->
  (t -> IO ()) ->
  TestTree
defaultTestSimple desc runner args assertResults = testCase desc $ do
  results <- runner args
  assertResults results

blockTestSimple ::
  TestName ->
  (List String -> IO a) ->
  List String ->
  (a -> IO ()) ->
  TestTree
blockTestSimple desc runner args assertResults = testCase (blockDesc desc) $ do
  results <- runner (blockArgs args)
  assertResults results

bufferTestSimple ::
  TestName ->
  (List String -> IO a) ->
  List String ->
  (a -> IO ()) ->
  TestTree
bufferTestSimple desc runner args assertResults = testCase (bufferDesc desc) $ do
  results <- runner (bufferArgs args)
  assertResults results

defaultTestSetup ::
  TestName ->
  (List String -> IO a) ->
  IO (List String, r) ->
  ((a, r) -> IO ()) ->
  TestTree
defaultTestSetup desc runner mkArgs assertResults = testCase desc $ do
  (args, extra) <- mkArgs
  results <- runner args
  assertResults (results, extra)

blockTestSetup ::
  TestName ->
  (List String -> IO a) ->
  IO (List String, r) ->
  ((a, r) -> IO ()) ->
  TestTree
blockTestSetup desc runner mkArgs assertResults = testCase (blockDesc desc) $ do
  (args, extra) <- mkArgs
  results <- runner (blockArgs args)
  assertResults (results, extra)

bufferTestSetup ::
  TestName ->
  (List String -> IO a) ->
  IO (List String, r) ->
  ((a, r) -> IO ()) ->
  TestTree
bufferTestSetup desc runner mkArgs assertResults = testCase (bufferDesc desc) $ do
  (args, extra) <- mkArgs
  results <- runner (bufferArgs args)
  assertResults (results, extra)

blockDesc :: String -> String
blockDesc = (++ " (block)")

blockArgs :: List String -> List String
blockArgs = (++ ["--command-log-read-strategy", "block"])

bufferDesc :: String -> String
bufferDesc = (++ " (block-line-buffer)")

bufferArgs :: List String -> List String
bufferArgs = (++ ["--command-log-read-strategy", "block-line-buffer"])

-- | Hedgehog option for ReadStrategy tests.
data ReadStrategyOpt
  = ReadStrategyBlock
  | ReadStrategyBlockLineBuffer
  | ReadStrategyAll
  deriving stock (Eq, Show)

instance IsOption ReadStrategyOpt where
  defaultValue = ReadStrategyBlock
  parseValue "block" = pure ReadStrategyBlock
  parseValue "block-line-buffer" = pure ReadStrategyBlockLineBuffer
  parseValue "all" = pure ReadStrategyAll
  parseValue bad =
    fail
      $ Utils.fmtUnrecognizedError
        "read-strategy"
        readStrategyStr
        bad
  optionName = Tagged "read-strategy"
  optionHelp = Tagged "Runs tests with specified command-log read strategy"
  optionCLParser =
    mkOptionCLParser (OA.metavar readStrategyStr)

readStrategyStr :: (IsString a) => a
readStrategyStr = "(block | block-line-buffer | all)"
