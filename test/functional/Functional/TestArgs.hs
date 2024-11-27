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

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "configPath" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2 a3) ->
        fmap
          (\b -> MkTestArgs b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "rootDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2 a3) ->
        fmap
          (\b -> MkTestArgs a1 b a3)
          (f a2)
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
      $ \f (MkTestArgs a1 a2 a3) ->
        fmap
          (\b -> MkTestArgs a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}
