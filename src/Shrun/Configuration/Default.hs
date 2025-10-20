module Shrun.Configuration.Default
  ( Default (..),
    fromMaybe,
    (<.>),
  )
where

import Shrun.Prelude hiding (fromMaybe)

-- | For types with a default value. In general, instances should be "simple"
-- i.e. no instances for aggregate TTG types (e.g. FileLogging) as complexity
-- jumps quickly.
class Default a where
  def :: a

instance Default (Maybe a) where
  def = Nothing

instance Default [a] where
  def = []

instance Default (Seq a) where
  def = Empty

fromMaybe :: (Default a) => Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = def

-- | Like '(<>?)' except we extract a result via 'fromDefault'.
(<.>) :: (Default a) => Maybe a -> Maybe a -> a
x <.> y = fromMaybe (x <|> y)

infixr 6 <.>
