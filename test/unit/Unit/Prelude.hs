module Unit.Prelude
  ( module X,
  )
where

import Hedgehog as X
  ( Gen,
    GenBase,
    MonadGen,
    Property,
    PropertyName,
    PropertyT,
    annotate,
    annotateShow,
    assert,
    diff,
    forAll,
    property,
    withTests,
    (===),
  )
import Shrun.Prelude as X
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.Hedgehog as X (testPropertyNamed)
