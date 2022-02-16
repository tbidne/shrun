-- | Custom prelude. The idea is to:
--
-- * Re-export useful prelude functions/types
-- * Export various functions/types from base
-- * Export new functions meant to address prelude limitations
--   (e.g. total replacements for partial functions).
--
-- This is not a comprehensive replacement for Prelude, just the
-- functionality needed for this application. Thus it is natural to
-- add new functionality/exports here over time.
--
-- @since 0.1.0.0
module ShellRun.Prelude
  ( -- * Total versions of partial functions
    headMaybe,

    -- * Misc utilities
    maybeToEither,
    monoBimap,

    -- * 'Text' replacements for 'P.String' functions.
    error,
    showt,
    readFileUtf8Lenient,
    writeFileUtf8,
    appendFileUtf8,

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,

    -- * Refined Aliases
    RNonNegative,
    RPositive,

    -- * Prelude exports
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (..),
    Applicative (..),
    (<**>),
  )
import Control.Monad as X
  ( Monad (..),
    join,
    void,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Control.Monad.Writer as X (MonadWriter (..), WriterT (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.ByteString qualified as BS
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
import Data.List as X (filter, zip)
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ordering (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Text.IO as X (putStr, putStrLn)
import Data.Traversable as X (Traversable (..))
import Refined (NonNegative, Positive, Refined)
import Prelude as X
  ( Bool (..),
    Bounded (..),
    Char,
    Eq (..),
    FilePath,
    IO,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    any,
    const,
    flip,
    fromIntegral,
    fst,
    not,
    otherwise,
    print,
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

-- | Strictly reads a file and leniently converts the contents to UTF8.
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient =
  fmap (TextEnc.decodeUtf8With TextEncErr.lenientDecode)
    . BS.readFile

-- | Writes the text contents to the file.
appendFileUtf8 :: FilePath -> Text -> IO ()
appendFileUtf8 fp = BS.appendFile fp . TextEnc.encodeUtf8

-- | Writes the text contents to the file.
writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp = BS.writeFile fp . TextEnc.encodeUtf8

-- | 'Text' version of 'P.show'.
showt :: P.Show a => a -> Text
showt = T.pack . P.show

-- | 'Text' version of 'error'.
error :: Text -> a
error = P.error . T.unpack

-- | Safe @head@.
--
-- >>> headMaybe [1,2,3]
-- Just 1
--
-- >>> headMaybe []
-- Nothing
headMaybe :: List a -> Maybe a
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

-- | Alias for '[]'.
--
-- @since 0.1.0.0
type List = []

-- | Alias for (,).
--
-- @since 0.1.0.0
type Tuple2 = (,)

-- | Alias for (,,).
--
-- @since 0.1.0.0
type Tuple3 = (,,)

-- | Alias for 'Refined' 'NonNegative' 'Int'.
--
-- @since 0.1.0.0
type RNonNegative = Refined NonNegative Int

-- | Alias for 'Refined' 'Positive' Int'.
--
-- @since 0.1.0.0
type RPositive = Refined Positive Int
