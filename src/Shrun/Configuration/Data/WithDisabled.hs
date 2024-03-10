{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (..),

    -- * Construction
    Shrun.Configuration.Data.WithDisabled.fromMaybe,

    -- * Elimination
    toMaybe,
    fromWithDisabled,

    -- * Misc
    (<>?),

    -- * Optics
    _With,
    _Without,
    _Disabled,
  )
where

import Shrun.Prelude

-- | Like Maybe but adds an extra constructor representing a "disabled" state.
-- The idea is that both CLI Args and Toml and have optional fields, but
-- the CLI can also be "disabled", which overrides everything.
--
-- The semigroup is similar to Maybe's:
--
-- - Identity: 'Without'
-- - 'With' is left-biased.
-- - ('Without', 'Without') forms a normal submonoid, in particular:
--
-- @
--   'Without' <> _ === 'Without' === _ <> 'Without'
-- @
data WithDisabled a
  = -- | The field.
    With a
  | -- | Missing.
    Without
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

makePrisms ''WithDisabled

instance Foldable WithDisabled where
  foldr f e (With x) = f x e
  foldr _ e Without = e
  foldr _ e Disabled = e

instance Applicative WithDisabled where
  pure = With

  Disabled <*> _ = Disabled
  _ <*> Disabled = Disabled
  Without <*> _ = Without
  _ <*> Without = Without
  With f <*> With x = With (f x)

instance Semigroup (WithDisabled a) where
  Disabled <> _ = Disabled
  _ <> Disabled = Disabled
  Without <> r = r
  l <> _ = l

instance Monoid (WithDisabled a) where
  mempty = Without

-- | 'With' -> 'Just', o/w -> 'Nothing'.
toMaybe :: WithDisabled a -> Maybe a
toMaybe (With x) = Just x
toMaybe _ = Nothing

-- | 'Nothing' -> 'Without', 'Just' -> 'With'.
fromMaybe :: Maybe a -> WithDisabled a
fromMaybe (Just x) = With x
fromMaybe Nothing = Without

-- | Eliminates 'WithDisabled'.
fromWithDisabled :: a -> WithDisabled a -> a
fromWithDisabled _ (With y) = y
fromWithDisabled x _ = x

-- | @l <>? r@ lifts 'Maybe' @r@ into a 'WithDisabled' per
-- 'Shrun.Configuration.Data.WithDisabled.fromMaybe' then runs the 'Semigroup'.
(<>?) :: WithDisabled a -> Maybe a -> WithDisabled a
wd <>? m = wd <> Shrun.Configuration.Data.WithDisabled.fromMaybe m

infixr 6 <>?
