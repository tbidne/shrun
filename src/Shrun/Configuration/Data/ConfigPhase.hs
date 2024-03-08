-- | Higher-Kinded data with type families for "phased-data" approach.
module Shrun.Configuration.Data.ConfigPhase
  ( -- * Types
    ConfigPhase (..),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
  )
where

import Shrun.Configuration.Data.WithDisable (WithDisable)
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
