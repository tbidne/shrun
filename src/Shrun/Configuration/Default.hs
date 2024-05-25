module Shrun.Configuration.Default
  ( Default (..),
  )
where

import Shrun.Prelude

-- | For types with a default value. In general, instances should be "simple"
-- i.e. no instances for aggregate TTG types (e.g. FileLogging) as complexity
-- jumps quickly.
class Default a where
  def :: a

instance Default (Maybe a) where
  def = Nothing
