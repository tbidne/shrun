-- | Higher-Kinded data with type families for "phased-data" approach.
module Shrun.Configuration.Data.ConfigPhase
  ( -- * Types
    ConfigPhase (..),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    BoolF,
  )
where

import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

-- | Data "phases" related to configuration.
data ConfigPhase
  = -- | Args phase.
    ConfigPhaseArgs
  | -- | Toml phase.
    ConfigPhaseToml
  | -- | Merged args + toml phase.
    ConfigPhaseMerged
  | -- | Env created from MergedConfig
    ConfigPhaseEnv

-- | General type family representing:
--
-- - Args: Maybe w/ disable flag
-- - Toml: Maybe
-- - Merged: Definite
type ConfigPhaseF :: ConfigPhase -> Type -> Type
type family ConfigPhaseF p a where
  ConfigPhaseF ConfigPhaseArgs a = WithDisabled a
  ConfigPhaseF ConfigPhaseToml a = Maybe a
  ConfigPhaseF ConfigPhaseMerged a = a
  ConfigPhaseF ConfigPhaseEnv a = a

-- | General type family representing:
--
-- - Args: Maybe w/ disable flag
-- - Toml: Maybe
-- - Merged: Maybe
type ConfigPhaseMaybeF :: ConfigPhase -> Type -> Type
type family ConfigPhaseMaybeF p a where
  ConfigPhaseMaybeF ConfigPhaseArgs a = WithDisabled a
  ConfigPhaseMaybeF ConfigPhaseToml a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseMerged a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseEnv a = Maybe a

-- | General type family representing a bool:
--
-- - Args: WithDisabled () (isomorphic to Disabled | Bool)
-- - Toml: Maybe Bool
-- - Merged: Bool
type BoolF :: ConfigPhase -> Type
type family BoolF p where
  BoolF ConfigPhaseArgs = WithDisabled ()
  BoolF ConfigPhaseToml = Maybe Bool
  BoolF ConfigPhaseMerged = Bool
  BoolF ConfigPhaseEnv = Bool
