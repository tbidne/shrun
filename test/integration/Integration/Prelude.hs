{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Effects.FileSystem.Utils as X ((</>!))
import Effects.FileSystem.Utils qualified as FsUtils
import Hedgehog as X
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
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

makeFieldLabelsNoPrefix ''TestArgs

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
concatDirs = foldr FsUtils.combineFilePaths []

osExt :: FilePath -> FilePath
#if OSX
osExt = (<> "_osx")
#else
osExt = id
#endif
