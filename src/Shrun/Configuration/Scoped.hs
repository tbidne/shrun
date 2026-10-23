{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides config scoping.
module Shrun.Configuration.Scoped
  ( ConfigScoped (..),
    ConfigScope (..),
    retag,
  )
where

import Shrun.Prelude

-- | Configuration scope.
data ConfigScope
  = -- | All scope refers to all combined configs.
    ConfigScopeAll
  | -- | Global scope refers to "global configs" i.e. everything but cwd
    -- (e.g. xdg/config.toml, explicit -c args).
    ConfigScopeGlobal
  | -- | "Local configs" i.e. ./.shrun.toml and ./shrun.toml.
    ConfigScopeLocal

-- | Type with configuration scope index.
type ConfigScoped :: ConfigScope -> Type -> Type
newtype ConfigScoped s a = MkConfigScoped
  { unConfigScoped :: a
  }
  deriving stock (Functor)

makeFieldLabelsNoPrefix ''ConfigScoped

deriving newtype instance (Semigroup a) => Semigroup (ConfigScoped s a)

deriving newtype instance (Monoid a) => Monoid (ConfigScoped s a)

instance Applicative (ConfigScoped s) where
  pure = MkConfigScoped

  MkConfigScoped f <*> MkConfigScoped x = MkConfigScoped (f x)

instance Monad (ConfigScoped s) where
  MkConfigScoped x >>= k = k x

retag :: forall s1 s2 a. ConfigScoped s1 a -> ConfigScoped s2 a
retag (MkConfigScoped x) = MkConfigScoped x
