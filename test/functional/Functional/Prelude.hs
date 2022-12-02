module Functional.Prelude
  ( module X,
  )
where

import Shrun.Prelude as X
import Test.Tasty as X (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit as X (Assertion, testCase, (@=?))
