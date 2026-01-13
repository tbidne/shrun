module Shrun.Data.Result
  ( Result (..),
  )
where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Category ((.))
import Control.Monad (Monad ((>>=)), MonadFail (fail))
import Data.Eq (Eq)
import Data.Foldable (Foldable (foldr))
import Data.Functor (Functor, (<$>))
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString (fromString))
import Data.Traversable (Traversable (sequenceA, traverse))
import Text.Show (Show)

-- | Either with custom MonadFail and fail-fast Semigroup instances.
data Result e a
  = Err e
  | Ok a
  deriving stock (Eq, Functor, Show)

instance (Semigroup a) => Semigroup (Result e a) where
  Err x <> _ = Err x
  _ <> Err y = Err y
  Ok x <> Ok y = Ok (x <> y)

instance (Monoid a) => Monoid (Result e a) where
  mempty = Ok mempty

instance Applicative (Result e) where
  pure = Ok

  Err x <*> _ = Err x
  _ <*> Err x = Err x
  Ok f <*> Ok x = Ok (f x)

instance Monad (Result e) where
  Err x >>= _ = Err x
  Ok x >>= f = f x

instance Foldable (Result e) where
  foldr _ e (Err _) = e
  foldr f e (Ok x) = f x e

instance Traversable (Result e) where
  sequenceA (Err x) = pure (Err x)
  sequenceA (Ok x) = Ok <$> x

  traverse _ (Err x) = pure (Err x)
  traverse f (Ok x) = Ok <$> f x

instance (IsString e) => MonadFail (Result e) where
  fail = Err . fromString
