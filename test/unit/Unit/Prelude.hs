{-# LANGUAGE CPP #-}

module Unit.Prelude
  ( module X,
    testPropertyCompat,
  )
where

import Hedgehog as X
  ( Gen,
    GenBase,
    MonadGen,
    Property,
    PropertyName,
    PropertyT,
    property,
    withTests,
    (===),
  )
import ShellRun.Prelude as X
import Test.Tasty as X (TestName, TestTree)
import Test.Tasty.HUnit as X (Assertion, (@=?))
import Test.Tasty.Hedgehog qualified as TastyH

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TastyH.testPropertyNamed
#else
testPropertyCompat tn _ = TastyH.testProperty tn
#endif
