{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Functional.Prelude

data TestArgs = MkTestArgs
  { tmpDir :: FilePath,
    legendPath :: FilePath
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''TestArgs

newtype TmpDir = MkTmpDir {unTmpDir :: FilePath}
  deriving (Show)

makeFieldLabelsNoPrefix ''TmpDir

newtype LegendPath = MkLegendPath {unLegendPath :: FilePath}
  deriving (Show)

makeFieldLabelsNoPrefix ''LegendPath
