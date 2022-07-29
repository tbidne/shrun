{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Functional.Prelude

data TestArgs = MkTestArgs
  { rootDir :: FilePath,
    tmpDir :: FilePath,
    configPath :: FilePath
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''TestArgs
