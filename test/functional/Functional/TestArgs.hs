{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Functional.Prelude

data TestArgs = MkTestArgs
  { rootDir :: OsPath,
    tmpDir :: OsPath,
    configPath :: OsPath
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''TestArgs
