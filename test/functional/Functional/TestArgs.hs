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

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "rootDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTestArgs _rootDir _tmpDir _configPath) ->
          fmap
            (\rootDir' -> MkTestArgs rootDir' _tmpDir _configPath)
            (f _rootDir)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "tmpDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTestArgs _rootDir _tmpDir _configPath) ->
          fmap
            (\tmpDir' -> MkTestArgs _rootDir tmpDir' _configPath)
            (f _tmpDir)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "configPath" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTestArgs _rootDir _tmpDir _configPath) ->
          fmap
            (MkTestArgs _rootDir _tmpDir)
            (f _configPath)
  {-# INLINE labelOptic #-}
