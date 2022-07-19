module Integration.Prelude
  ( module X,
  )
where

import Shrun.Prelude as X
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?))
