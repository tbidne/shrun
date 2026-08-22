{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Shrun.Prelude

data TestArgs = MkTestArgs
  { -- | Path to test toml file i.e. <shrun_repo>/test/functional/config.toml.
    configPath :: OsPath,
    -- | <tmp>/shrun.
    rootDir :: OsPath,
    -- | <tmp>/shrun/test/functional.
    tmpDir :: OsPath
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''TestArgs
