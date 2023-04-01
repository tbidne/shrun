-- | Provides framework for "phased" data. This is a generalization of
-- the "Higher-kinded data" approach where instead of merely annotating a
-- data type with some constructor e.g.
--
-- @
-- data Foo f = MkFoo
--   { bar :: f Bool,
--     baz :: f String,
--     ...
--   }
-- @
--
-- we use a type family:
--
-- @
-- data Foo p = MkFoo
--   { bar :: Fam1 p,
--     baz :: Fam2 p,
--   ...
--   }
-- @
--
-- Not only does this allow fields to vary independently, it also means
-- we can avoid clunky wrappers like @Identity@.
--
-- We do this so that we can represent data that "evolves" e.g. we may
-- initially parse a data type as Foo1 (phase 1) but then further process
-- this data type into Foo2 (phase 2). Using this phased approach avoids
-- duplication of constructors and fields.
module Shrun.Data.Phase
  ( Phase (..),
    AdvancePhase (..),
  )
where

import Shrun.Prelude

-- | Index for data that has a "phased" evolution.
data Phase
  = Phase1
  | Phase2
  deriving stock (Eq, Show)

-- | Advances phased data.
class AdvancePhase a where
  -- | The next phase.
  type NextPhase a

  -- | Advances the data.
  advancePhase :: a -> NextPhase a
