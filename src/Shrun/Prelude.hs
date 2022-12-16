{-# LANGUAGE CPP #-}

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
module Shrun.Prelude
  ( -- * Total versions of partial functions
    headMaybe,

    -- * Misc utilities
    (<<$>>),
    (.>),
    monoBimap,
    m2b,

    -- * 'Text' replacements for 'P.String' functions.
    error,
    showt,

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
import Control.Monad as X
  ( Monad (..),
    forever,
    join,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Catch as X (MonadCatch, MonadMask (..), MonadThrow, throwM)
import Control.Monad.Fail as X (MonadFail (..))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Control.Monad.Writer as X (MonadWriter (..), WriterT (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Char as X (Char)
import Data.Either as X (Either (..))
import Data.Eq as X (Eq (..))
import Data.Foldable as X
  ( Foldable (..),
    any,
    length,
    traverse_,
  )
import Data.Function as X (const, flip, id, ($), (&), (.))
import Data.Functor as X
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
  )
import Data.IORef as X (IORef)
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter, replicate, zip, (++))
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..), Ordering (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (String)
import Data.Text as X (Text, pack, unpack)
import Data.Text qualified as T
import Data.Traversable as X (Traversable (..), for)
import Data.Tuple as X (fst, snd)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality as X (type (~))
#endif
import Data.Void as X (Void, absurd)
import Effects.MonadCallStack as X
  ( MonadCallStack (throwWithCallStack),
    catch,
    displayCallStack,
    try,
  )
import Effects.MonadFs as X
  ( MonadFsReader (readFile),
    MonadFsWriter,
    appendFileUtf8,
    hFlush,
    hPutUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    removeDirectoryIfExists,
    removeFileIfExists,
    writeFileUtf8,
  )
import Effects.MonadIORef as X
  ( MonadIORef
      ( modifyIORef',
        newIORef,
        readIORef,
        writeIORef
      ),
  )
import Effects.MonadSTM as X
  ( MonadSTM,
    MonadTBQueue (flushTBQueueM, newTBQueueM, readTBQueueM, writeTBQueueM),
    MonadTVar (modifyTVarM', newTVarM, readTVarM),
    TBQueue,
    TVar,
  )
import Effects.MonadTerminal as X
  ( MonadTerminal,
    putStr,
    putStrLn,
    putText,
    putTextLn,
  )
import Effects.MonadThread as X (MonadQSem, MonadThread)
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Err as X (undefined)
import GHC.Float as X (Double (..), Float (..))
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num (..))
import GHC.Real as X (Integral (..), fromIntegral, truncate)
import GHC.Show as X (Show (..))
import GHC.Stack as X (HasCallStack)
import Numeric.Algebra as X (NonZero (..))
import Optics.Core as X
  ( A_Getter,
    AffineTraversal',
    An_AffineFold,
    An_AffineTraversal,
    Getter,
    Is,
    Iso',
    LabelOptic (..),
    Lens',
    Optic',
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
    (?~),
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
import Optics.Core.Extras as X (is)
import Optics.TH as X
  ( generateUpdateableOptics,
    makeFieldLabelsNoPrefix,
    makeFieldLabelsWith,
    makePrisms,
    noPrefixFieldLabels,
  )
import Refined as X (Refined)
import System.Console.Regions as X (ConsoleRegion, RegionLayout (Linear))
import System.FilePath as X ((</>))
import System.IO as X (FilePath, Handle, IO, IOMode (..), print)
import TOML as X
  ( DecodeTOML (..),
    Decoder,
    TOMLError (..),
    Value,
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
import UnliftIO as X (MonadUnliftIO (..))
import UnliftIO.Environment as X (withArgs)
import UnliftIO.Exception as X
  ( Exception (..),
    IOException,
    SomeException,
    bracket,
    catchAny,
    catchIO,
    finally,
    onException,
    throwIO,
    throwString,
    tryAny,
  )
import Prelude as X (seq)
import Prelude qualified as P

-- $setup
-- >>> import Data.String (String)
-- >>> :set -XNoOverloadedLists

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

-- | Transforms a maybe to a bool
--
-- @since 0.1
m2b :: Maybe a -> Bool
m2b (Just _) = True
m2b Nothing = False
{-# INLINE m2b #-}

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
