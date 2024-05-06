module Shrun.Configuration.Default
  ( Default (..),
  )
where

-- | For types with a default value. In general, instances should be "simple"
-- i.e. no instances for aggregate TTG types (e.g. FileLogging) as complexity
-- jumps quickly.
class Default a where
  def :: a
