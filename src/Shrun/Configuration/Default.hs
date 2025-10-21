module Shrun.Configuration.Default
  ( Default (..),
    fromMaybe,
    (<.>),
  )
where

import Shrun.Prelude hiding (fromMaybe)

-- | For types with a default value. In general, instances should be "simple"
-- i.e. no instances for aggregate TTG types (e.g. FileLogging) except for
-- CLI args, as complexity jumps quickly.
--
-- For the most part, types should only have instances for one of
-- Default xor (Semigroup/Monoid or Alternative) i.e. if types /do/ support
-- some sort of algebra, the latter is preferred. Default exists when
-- no algebra is sensible.
class Default a where
  def :: a

fromMaybe :: (Default a) => Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = def

-- | Like '(<>?)' except we extract a result via 'fromDefault'.
(<.>) :: (Default a) => Maybe a -> Maybe a -> a
x <.> y = fromMaybe (x <|> y)

infixr 6 <.>
