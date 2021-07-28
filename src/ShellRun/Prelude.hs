-- | Custom prelude. The idea is to:
--
-- * Re-export useful prelude functions/types
-- * Export various functions/types from base
-- * Export new functions meant to address prelude limitations (e.g. total replacements for partial functions).
--
-- This is not a comprehensive replacement for Prelude, just the functionality needed for this application. Thus it is natural to add new functionality/exports here over time.
module ShellRun.Prelude
  ( -- * Total versions of partial functions
    NE.head,
    headMaybe,

    -- * Misc utilities
    maybeToEither,
    monoBimap,

    -- * 'Text' replacements for 'String' functions.
    error,
    readFile,
    showt,

    -- * Prelude exports
    module X,
  )
where

import Control.Applicative as X
  ( Applicative (..),
    liftA2,
    (<**>),
    (<|>),
  )
import Control.Monad as X
  ( Monad (..),
    join,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..))
import Control.Monad.Trans as X (MonadTrans (..))
import Control.Monad.Writer as X (MonadWriter (..), WriterT (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Either as X (Either (..))
import Data.Foldable as X
  ( Foldable
      ( fold,
        foldMap,
        foldMap',
        foldl',
        foldr
      ),
    length,
    traverse_,
  )
import Data.Functor as X
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
  )
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe as X (Maybe (..))
import Data.Monoid as X (Monoid (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.IO as X (putStr, putStrLn)
import Data.Traversable as X (Traversable (..))
import Prelude as X
  ( Bool (..),
    Bounded (..),
    Eq (..),
    IO,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    const,
    flip,
    fromIntegral,
    fst,
    not,
    otherwise,
    replicate,
    snd,
    ($),
    (&&),
    (.),
    (||),
  )
import Prelude qualified as P

-- $setup
-- >>> import Data.String (String)

readFile :: Text -> IO Text
readFile = fmap T.pack . P.readFile . T.unpack

showt :: P.Show a => a -> Text
showt = T.pack . P.show

error :: Text -> a
error = P.error . T.unpack

-- | Safe @head@.
--
-- >>> headMaybe [1,2,3]
-- Just 1
--
-- >>> headMaybe []
-- Nothing
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Transforms 'Maybe' to 'Either'.
--
-- >>> maybeToEither () (Nothing :: Maybe String)
-- Left ()
--
-- >>> maybeToEither () (Just "success" :: Maybe String)
-- Right "success"
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x

-- | Convenience function for mapping @(a -> b)@ over a monomorphic bifunctor.
--
-- Example:
--
-- >>> monoBimap length ("hey","listen")
-- (3,6)
monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = bimap f f
