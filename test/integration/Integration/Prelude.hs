{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Prelude
  ( module X,
    TestArgs (..),
  )
where

import Shrun.Prelude as X
import Test.Tasty as X
  ( TestName,
    TestTree,
    defaultMain,
    testGroup,
    withResource,
  )
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )

data TestArgs = MkTestArgs
  { rootTmpDir :: FilePath,
    workingTmpDir :: FilePath
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TestArgs
