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
-- @since 0.1
module ShellRun.Prelude
  ( -- * Total versions of partial functions
    headMaybe,

    -- * Misc utilities
    (<<$>>),
    (.>),
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

    -- * Prelude exports
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (..),
    Applicative (..),
    (<**>),
  )
import Control.Concurrent as X (threadDelay)
import Control.Concurrent.STM as X (atomically)
import Control.Concurrent.STM.TBQueue as X
  ( TBQueue,
    flushTBQueue,
    newTBQueue,
    tryReadTBQueue,
    writeTBQueue,
  )
import Control.Concurrent.STM.TVar as X (TVar)
import Control.Monad as X
  ( Monad (..),
    forever,
    join,
    void,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Catch as X (MonadCatch, MonadMask (..), MonadThrow)
import Control.Monad.Fail as X (MonadFail (..))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Control.Monad.Writer as X (MonadWriter (..), WriterT (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.ByteString as X (ByteString)
import Data.ByteString qualified as BS
import Data.Either as X (Either (..))
import Data.Foldable as X
  ( Foldable (..),
    length,
    traverse_,
  )
import Data.Function as X ((&))
import Data.Functor as X
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
  )
import Data.IORef as X (IORef)
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter, zip, (++))
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ordering (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (String)
import Data.Text as X (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Text.IO as X (putStr, putStrLn)
import Data.Traversable as X (Traversable (..), for)
import Data.Void as X (Void, absurd)
import GHC.Float as X (Double (..), Float (..))
import GHC.Generics as X (Generic)
import GHC.Natural as X (Natural)
import GHC.Stack as X (HasCallStack)
import Numeric.Algebra as X (NonZero (..))
import Optics.Core as X
  ( A_Getter,
    Getter,
    Iso',
    LabelOptic (..),
    Lens',
    Prism,
    Prism',
    iso,
    over',
    preview,
    review,
    set',
    to,
    view,
    (#),
    (%),
    (%!~),
    (%?),
    (.~),
    (^.),
    (^?),
    _1,
    _2,
    _3,
    _Just,
    _Left,
    _Nothing,
    _Right,
  )
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrisms)
import Refined as X (Refined)
import TOML as X
  ( DecodeTOML (..),
    Decoder,
    TOMLError (..),
    Value (..),
    decode,
    decodeWith,
    getArrayOf,
    getField,
    getFieldOpt,
    getFieldOptWith,
    getFieldWith,
    invalidValue,
    makeDecoder,
    renderTOMLError,
    runDecoder,
    typeMismatch,
  )
import UnliftIO as X
  ( Exception (..),
    MonadUnliftIO (..),
    SomeException,
    catch,
    catchAny,
    catchIO,
    onException,
    throwIO,
    throwString,
    try,
    tryAny,
  )
import Prelude as X
  ( Bool (..),
    Bounded (..),
    Char,
    Enum (..),
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
    id,
    not,
    otherwise,
    print,
    replicate,
    seq,
    snd,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )
import Prelude qualified as P

-- $setup
-- >>> import Data.String (String)
-- >>> :set -XNoOverloadedLists

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient =
  fmap (TextEnc.decodeUtf8With TextEncErr.lenientDecode)
    . BS.readFile
{-# INLINEABLE readFileUtf8Lenient #-}

-- | Appends the text contents to the file.
--
-- @since 0.1
appendFileUtf8 :: FilePath -> Text -> IO ()
appendFileUtf8 fp = BS.appendFile fp . TextEnc.encodeUtf8
{-# INLINEABLE appendFileUtf8 #-}

-- | Writes the text contents to the file.
--
-- @since 0.1
writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp = BS.writeFile fp . TextEnc.encodeUtf8
{-# INLINEABLE writeFileUtf8 #-}

-- | 'Text' version of 'P.show'.
--
-- @since 0.1
showt :: P.Show a => a -> Text
showt = T.pack . P.show
{-# INLINEABLE showt #-}

-- | 'Text' version of 'error'.
--
-- @since 0.1
error :: HasCallStack => Text -> a
error = P.error . T.unpack
{-# INLINEABLE error #-}

-- | Safe @head@.
--
-- >>> headMaybe [1,2,3]
-- Just 1
--
-- >>> headMaybe []
-- Nothing
--
-- @since 0.1
headMaybe :: List a -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
{-# INLINEABLE headMaybe #-}

-- | Convenience function for mapping @(a -> b)@ over a monomorphic bifunctor.
--
-- Example:
--
-- >>> monoBimap length ("hey","listen")
-- (3,6)
--
-- @since 0.1
monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = bimap f f
{-# INLINEABLE monoBimap #-}

-- | Lifted fmap.
--
-- >>> not <<$>> [Just True, Nothing, Just False]
-- [Just False,Nothing,Just True]
--
-- @since 0.1
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
{-# INLINEABLE (<<$>>) #-}

infixl 4 <<$>>

-- | Flipped '(.)'
--
-- @since 0.5
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f
{-# INLINE (.>) #-}

infixr 9 .>

-- | Alias for [].
--
-- @since 0.1
type List = []

-- | Alias for (,).
--
-- @since 0.1
type Tuple2 = (,)

-- | Alias for (,,).
--
-- @since 0.1
type Tuple3 = (,,)
