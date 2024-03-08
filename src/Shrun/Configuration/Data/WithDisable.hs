{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.WithDisable
  ( WithDisable (..),

    -- * Elimination
    defaultIfDisabled,
    emptyIfDisabled,

    -- * Functions
    alternativeDefault,
    alternativeEmpty,

    -- * Optics
    _With,
    _Disabled,
  )
where

import Shrun.Prelude

-- | Adds a "disable" flag to some data. Though this is isomorphic to
-- Maybe, we create a new type to be clearer about provenance. For instance,
-- WithDisable (Maybe a) has much clearer meaning than Maybe (Maybe a)
-- ("which level means what?").
--
-- The semigroup instance is based on an inner alternative i.e. With uses
-- the inner alternative (With empty is the identity) and (Disabled, mempty) is
-- a normal submonoid (i.e. Disabled <> x === Disabled === x <> Disabled).
data WithDisable a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

makePrisms ''WithDisable

instance Foldable WithDisable where
  foldr f e (With x) = f x e
  foldr _ e Disabled = e

instance Applicative WithDisable where
  pure = With

  Disabled <*> _ = Disabled
  _ <*> Disabled = Disabled
  With f <*> With x = With (f x)

instance (Alternative f) => Semigroup (WithDisable (f a)) where
  Disabled <> _ = Disabled
  _ <> Disabled = Disabled
  With l <> With r = With (l <|> r)

instance (Alternative f) => Monoid (WithDisable (f a)) where
  mempty = With empty

-- | Returns the data if it exists and is not disabled, otherwise returns
-- the default.
defaultIfDisabled :: (Foldable f) => a -> WithDisable (f a) -> a
defaultIfDisabled x Disabled = x
defaultIfDisabled x (With y) = fromFoldable x y

-- | Returns empty if the data is disabled or it does not exist.
emptyIfDisabled :: (Alternative f) => WithDisable (f a) -> f a
emptyIfDisabled Disabled = empty
emptyIfDisabled (With x) = x

-- | Morally returns @l <|> r@ or the default, taking Disabled into account.
--
-- @
--   alternativeDefault x Disabled _ === x
--   alternativeDefault x (With l) r === fromFoldable x (l <|> r)
-- @
alternativeDefault ::
  (Alternative f, Foldable f) =>
  a ->
  WithDisable (f a) ->
  f a ->
  a
alternativeDefault defA args = fromFoldable defA . alternativeEmpty args

-- | Morally returns @l <|> r@, taking Disabled into account.
--
-- @
--   alternativeEmpty Disabled _ === empty
--   alternativeEmpty (With l) r === l <|> r
-- @
alternativeEmpty ::
  (Alternative f) =>
  -- | l
  WithDisable (f a) ->
  -- | r
  f a ->
  -- | l <|> r
  f a
alternativeEmpty l r = emptyIfDisabled $ l <> pure r
