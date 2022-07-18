{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Data.String (IsString)
import Functional.Prelude

data TestArgs = MkTestArgs
  { tmpDir :: FilePath,
    configPath :: FilePath
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''TestArgs

newtype TmpDir = MkTmpDir {unTmpDir :: FilePath}
  deriving stock (Show)
  deriving (IsString) via String

makeFieldLabelsNoPrefix ''TmpDir

newtype ConfigPath = MkConfigPath {unConfigPath :: FilePath}
  deriving stock (Show)
  deriving (IsString) via String

makeFieldLabelsNoPrefix ''ConfigPath
