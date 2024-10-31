{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Prelude
  ( module X,
    TestArgs (..),
    getExampleConfig,
    getExampleConfigOS,
    getIntConfig,
    getIntConfigOS,
    concatDirs,
  )
where

import FileSystem.OsPath as X
  ( combineFilePaths,
    unsafeDecode,
    unsafeEncode,
    (</>!),
  )
import Hedgehog as X
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
    assert,
    property,
    withTests,
    (===),
  )
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
import Test.Tasty.Hedgehog as X (testPropertyNamed)

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
      $ \f
         (MkTestArgs _rootTmpDir _workingTmpDir) ->
          fmap
            (`MkTestArgs` _workingTmpDir)
            (f _rootTmpDir)
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
      $ \f
         (MkTestArgs _rootTmpDir _workingTmpDir) ->
          fmap
            (MkTestArgs _rootTmpDir)
            (f _workingTmpDir)
  {-# INLINE labelOptic #-}

-- | Retrieves file path from the examples directory, potentially appending
-- the os onto the filename (e.g. osx).
getExampleConfigOS :: FilePath -> FilePath
getExampleConfigOS fileName =
  concatDirs ["examples", osExt fileName]
    <> ".toml"

-- | Retrieves file path from the examples directory.
getExampleConfig :: FilePath -> FilePath
getExampleConfig fileName =
  concatDirs ["examples", fileName]
    <> ".toml"

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

concatDirs :: [FilePath] -> FilePath
concatDirs = foldr combineFilePaths []

osExt :: FilePath -> FilePath
#if OSX
osExt = (<> "_osx")
#else
osExt = id
#endif
