{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Prelude
  ( module X,
    TestArgs (..),
    getExampleConfigOS,
    getIntConfig,
    getIntConfigOS,
    concatDirs,
    mkIdx,

    -- * Hedgehog
    testProp,
    testProp1,
    testPropN,
  )
where

import FileSystem.OsPath as X
  ( combineFilePaths,
    unsafeDecode,
    (</>!),
  )
import Hedgehog as X
  ( Property,
    PropertyName,
    PropertyT,
    TestLimit,
    annotate,
    annotateShow,
    assert,
    failure,
    property,
    withTests,
    (===),
  )
import Shrun.Command.Types (CommandIndex, fromPositive)
import Shrun.Prelude as X
import Test.Tasty as X
  ( TestName,
    TestTree,
    defaultMain,
    localOption,
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

data TestArgs = MkTestArgs
  { rootTmpDir :: OsPath,
    workingTmpDir :: OsPath
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "rootTmpDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2) ->
        fmap
          (\b -> MkTestArgs b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "workingTmpDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2) ->
        fmap
          (\b -> MkTestArgs a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | Retrieves file path from the examples directory, potentially appending
-- the os onto the filename (e.g. osx).
getExampleConfigOS :: FilePath

#if OSX
getExampleConfigOS = concatDirs ["test", "functional", "example_osx.toml"]
#else
getExampleConfigOS = concatDirs ["examples", "config.toml"]
#endif

-- | Retrieves file path from the integration directory, potentially appending
-- the os onto the filename (e.g. osx).
getIntConfigOS :: FilePath -> FilePath
getIntConfigOS fileName =
  concatDirs ["test", "integration", "toml", osExt fileName]
    <> ".toml"

-- | Retrieves file path from the integration directory.
getIntConfig :: FilePath -> FilePath
getIntConfig fileName =
  concatDirs ["test", "integration", "toml", fileName]
    <> ".toml"

concatDirs :: List FilePath -> FilePath
concatDirs = foldr combineFilePaths []

osExt :: FilePath -> FilePath
#if OSX
osExt = (<> "_osx")
#else
osExt = id
#endif

mkIdx :: Int -> CommandIndex
mkIdx = fromPositive . unsafePositive
