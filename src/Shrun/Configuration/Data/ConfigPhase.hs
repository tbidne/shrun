{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Higher-Kinded data with type families for "phased-data" approach.
module Shrun.Configuration.Data.ConfigPhase
  ( -- * Types
    ConfigPhase (..),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    WithDisable (..),
    emptyWithDisable,

    -- * Functions
    defaultIfDisabled,
    nothingIfDisabled,
    altDefault,
    altNothing,

    -- * Optics
    _With,
    _Disabled,
  )
where

import Shrun.Prelude

-- | Data "phases" related to configuration.
data ConfigPhase
  = -- | Args phase.
    ConfigPhaseArgs
  | -- | Toml phase.
    ConfigPhaseToml
  | -- | Merged args + toml phase.
    ConfigPhaseMerged

-- | General type family representing:
--
-- - Args: Maybe w/ disable flag
-- - Toml: Maybe
-- - Merged: Definite
type ConfigPhaseF :: ConfigPhase -> Type -> Type
type family ConfigPhaseF p a where
  ConfigPhaseF ConfigPhaseArgs a = WithDisable (Maybe a)
  ConfigPhaseF ConfigPhaseToml a = Maybe a
  ConfigPhaseF ConfigPhaseMerged a = a

-- | General type family representing:
--
-- - Args: Maybe w/ disable flag
-- - Toml: Maybe
-- - Merged: Maybe
type ConfigPhaseMaybeF :: ConfigPhase -> Type -> Type
type family ConfigPhaseMaybeF p a where
  ConfigPhaseMaybeF ConfigPhaseArgs a = WithDisable (Maybe a)
  ConfigPhaseMaybeF ConfigPhaseToml a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseMerged a = Maybe a

-- | Adds a "disable" flag to some data. Though this is isomorphic to
-- Maybe, we create a new type to be clearer about provenance. For instance,
-- WithDisable (Maybe a) has much clearer meaning than Maybe (Maybe a)
-- ("which level means what?").
data WithDisable a
  = -- | The field.
    With a
  | -- | Disabled.
    Disabled
  deriving stock (Eq, Functor, Show)

makePrisms ''WithDisable

-- | Initial WithDisable i.e. empty but not disabled.
emptyWithDisable :: (Alternative f) => WithDisable (f a)
emptyWithDisable = With empty

-- | Returns the data if it exists and is not disabled, otherwise returns
-- the default.
defaultIfDisabled :: a -> WithDisable (Maybe a) -> a
defaultIfDisabled x = fromMaybe x . nothingIfDisabled

-- | Returns nothing if the data is disabled or it does not exist.
nothingIfDisabled :: WithDisable (Maybe a) -> Maybe a
nothingIfDisabled = preview (_With % _Just)

-- | Morally returns @l <|> r@, if one exists and the disable flag is not
-- active. Otherwise returns the default.
altDefault :: a -> args -> Lens' args (WithDisable (Maybe a)) -> Maybe a -> a
altDefault defA args l = fromMaybe defA . altNothing args l

-- | Morally returns @l <|> r@, if one exists and the disable flag is not
-- active.
altNothing :: args -> Lens' args (WithDisable (Maybe a)) -> Maybe a -> Maybe a
altNothing args l r =
  case args ^. l of
    Disabled -> Nothing
    With x -> x <|> r
