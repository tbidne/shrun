module Unit.Prelude
  ( module X,

    -- * Hedgehog
    testProp,
    testProp1,
    testPropN,

    -- * Misc
    mkIdx,
  )
where

import Hedgehog as X
  ( Gen,
    GenBase,
    MonadGen,
    Property,
    PropertyName,
    PropertyT,
    TestLimit,
    annotate,
    annotateShow,
    assert,
    diff,
    failure,
    footnote,
    footnoteShow,
    forAll,
    property,
    withTests,
    (===),
  )
import Shrun.Command.Types (CommandIndex, fromPositive)
import Shrun.Prelude as X
import Test.Tasty as X (TestName, TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.Hedgehog as X
  ( HedgehogTestLimit (HedgehogTestLimit),
    testPropertyNamed,
  )

-- | Concise alias for @testPropertyNamed . property@
testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp name desc = testPropertyNamed name desc . property

-- | 'testProp' that only runs a single test. Used for when we'd really want
-- HUnit's testCase, but with a better diff.
testProp1 :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp1 = testPropN 1

-- | 'testProp' that runs for specified N times.
testPropN :: TestLimit -> TestName -> PropertyName -> PropertyT IO () -> TestTree
testPropN numTests name desc =
  -- NOTE: Have to use localOption here as it overrides withTests. That is,
  -- hedgehog's withTests has NO effect here, so don't use it!
  localOption (HedgehogTestLimit (Just numTests))
    . testProp name desc

mkIdx :: Int -> CommandIndex
mkIdx = fromPositive . unsafePositive
