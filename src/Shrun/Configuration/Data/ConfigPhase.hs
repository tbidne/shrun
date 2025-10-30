-- | Higher-Kinded data with type families for "phased-data" approach.
module Shrun.Configuration.Data.ConfigPhase
  ( -- * Types
    ConfigPhase (..),

    -- * Type families
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    ConfigPhaseDisabledMaybeF,
    LineTruncF,
    SwitchF,

    -- * Misc
    parseSwitch,
  )
where

import Shrun.Configuration.Data.Truncation
  ( LineTruncation,
    TruncRegion (TruncLine),
    Truncation,
  )
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
  ConfigPhaseF ConfigPhaseArgs a = Maybe a
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
  ConfigPhaseMaybeF ConfigPhaseArgs a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseToml a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseMerged a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseEnv a = Maybe a

type ConfigPhaseDisabledMaybeF :: ConfigPhase -> Type -> Type
type family ConfigPhaseDisabledMaybeF p a where
  ConfigPhaseDisabledMaybeF ConfigPhaseArgs a = Maybe (WithDisabled a)
  ConfigPhaseDisabledMaybeF ConfigPhaseToml a = Maybe (WithDisabled a)
  ConfigPhaseDisabledMaybeF ConfigPhaseMerged a = Maybe a
  ConfigPhaseDisabledMaybeF ConfigPhaseEnv a = Maybe a

-- | General type family representing a boolean switch for a type @t@ that
-- is isomorphic to Bool. Args and Toml distinguish explicit false
-- (Just 'false') vs. not given (Nothing).
--
-- - Args: Maybe t
-- - Toml: Maybe t
-- - Merged: t
-- - Env: t
type SwitchF :: ConfigPhase -> Type -> Type
type family SwitchF p t where
  SwitchF ConfigPhaseArgs t = Maybe t
  SwitchF ConfigPhaseToml t = Maybe t
  SwitchF ConfigPhaseMerged t = t
  SwitchF ConfigPhaseEnv t = t

-- | Line truncation is truly optional, the default being none.
type LineTruncF :: ConfigPhase -> Type
type family LineTruncF p where
  LineTruncF ConfigPhaseArgs = Maybe (WithDisabled LineTruncation)
  LineTruncF ConfigPhaseToml = Maybe (WithDisabled LineTruncation)
  LineTruncF ConfigPhaseMerged = Maybe (Truncation TruncLine)
  LineTruncF ConfigPhaseEnv = Maybe (Truncation TruncLine)

parseSwitch :: (MonadFail m) => Text -> m Bool
parseSwitch = \case
  "on" -> pure True
  "off" -> pure False
  other ->
    fail
      $ mconcat
        [ "Expected (on | off), received: '",
          unpack other,
          "'"
        ]
