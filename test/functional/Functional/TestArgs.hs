module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Functional.Prelude

data TestArgs = MkTestArgs
  { tTmpDir :: FilePath,
    tLegendPath :: FilePath
  }
  deriving (Show)

newtype TmpDir = MkTmpDir {unTmpDir :: FilePath}
  deriving (Show)

newtype LegendPath = MkLegendPath {unLegendPath :: FilePath}
  deriving (Show)
