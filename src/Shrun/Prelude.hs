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
module Shrun.Prelude
  ( -- * Total versions of partial functions
    headMaybe,

    -- * Misc utilities
    fromFoldable,
    (<<$>>),
    (.>),

    -- * 'Text' replacements for 'P.String' functions.
    showt,
    displayExceptiont,

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,

    -- * Prelude exports
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (*>), (<*>)),
    (<**>),
  )
import Control.Concurrent as X (threadDelay)
import Control.Monad as X
  ( Monad ((>>=)),
    forever,
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.Trans as X (MonadTrans (lift))
import Data.Bifunctor as X (Bifunctor)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X
  ( Bytes (MkBytes),
    FromInteger (afromInteger),
    Size (B),
    _MkBytes,
  )
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (fold, foldl', foldr, toList),
    any,
    for_,
    length,
    traverse_,
  )
import Data.Function as X (const, flip, id, ($), (&), (.))
import Data.Functor as X
  ( Functor (fmap),
    ($>),
    (<$>),
    (<&>),
  )
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter, replicate, zip, (++))
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord ((<), (<=), (>), (>=)), Ordering)
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq ((:<|), (:|>)))
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)), pattern IsEmpty)
import Data.String as X (String)
import Data.Text as X (Text, pack, unpack)
import Data.Text qualified as T
import Data.Traversable as X (Traversable (traverse), for)
import Data.Tuple as X (fst, snd)
import Data.Type.Equality as X (type (~))
import Data.Void as X (Void, absurd)
import Effects.Concurrent.Async as X (MonadAsync)
import Effects.Concurrent.STM as X
  ( MonadSTM,
    TBQueue,
    TVar,
    flushTBQueueA,
    modifyTVarA',
    newTBQueueA,
    newTVarA,
    readTBQueueA,
    readTVarA,
    writeTBQueueA,
    writeTVarA,
  )
import Effects.Concurrent.Thread as X (MonadThread)
import Effects.Exception as X
  ( Exception (displayException, fromException),
    ExceptionCS (MkExceptionCS),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    bracket,
    catchAny,
    catchCS,
    displayException,
    exitFailure,
    finally,
    mask,
    throwCS,
    throwM,
    try,
    tryAny,
  )
import Effects.FileSystem.FileReader as X
  ( MonadFileReader,
    decodeUtf8Lenient,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
  )
import Effects.FileSystem.FileWriter as X
  ( MonadFileWriter,
    appendFileUtf8,
    writeFileUtf8,
  )
import Effects.FileSystem.HandleReader as X (MonadHandleReader)
import Effects.FileSystem.HandleWriter as X
  ( MonadHandleWriter (hClose, hFlush, openBinaryFile),
    hPutUtf8,
  )
import Effects.FileSystem.PathReader as X
  ( MonadPathReader (doesDirectoryExist, doesFileExist, getFileSize),
    getXdgConfig,
    getXdgState,
  )
import Effects.FileSystem.PathWriter as X
  ( MonadPathWriter,
    removeDirectoryIfExists,
    removeFile,
    removeFileIfExists,
  )
import Effects.FileSystem.Utils as X (OsPath, osp, (</>))
import Effects.IORef as X
  ( IORef,
    MonadIORef
      ( atomicModifyIORef',
        modifyIORef',
        newIORef,
        readIORef,
        writeIORef
      ),
  )
import Effects.Optparse as X (MonadOptparse (execParser))
import Effects.Process.Typed as X (MonadTypedProcess, Process)
import Effects.System.Environment as X (MonadEnv (withArgs))
import Effects.System.Terminal as X
  ( MonadTerminal,
    putStr,
    putStrLn,
    putText,
    putTextLn,
  )
import Effects.Time as X (MonadTime)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (toEnum))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Real as X (Integral, fromIntegral, truncate)
import GHC.Show as X (Show (show, showsPrec))
import GHC.Stack as X (HasCallStack)
import Optics.Core as X
  ( A_Getter,
    A_Setter,
    AffineTraversal',
    An_AffineFold,
    An_AffineTraversal,
    Getter,
    Is,
    Iso',
    LabelOptic (labelOptic),
    Lens,
    Lens',
    NoIx,
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
import System.Console.Regions as X (ConsoleRegion, RegionLayout (Linear))
import System.IO as X (FilePath, Handle, IO, IOMode (AppendMode, WriteMode), print)
import TOML as X
  ( DecodeTOML (tomlDecoder),
    Decoder,
    TOMLError,
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
import Prelude as X (seq)

-- $setup
-- >>> import Data.String (String)
-- >>> :set -XNoOverloadedLists

-- | 'Text' version of 'show'.
showt :: (Show a) => a -> Text
showt = T.pack . show

-- | 'Text' version of 'displayException'.
displayExceptiont :: (Exception e) => e -> Text
displayExceptiont = T.pack . displayException

-- | Safe @head@.
--
-- >>> headMaybe [1,2,3]
-- Just 1
--
-- >>> headMaybe []
-- Nothing
headMaybe :: (Foldable f) => f a -> Maybe a
headMaybe = foldr (\x _ -> Just x) Nothing

-- | From foldable.
fromFoldable :: (Foldable f) => a -> f a -> a
fromFoldable x = fromMaybe x . headMaybe

-- | Lifted fmap.
--
-- >>> not <<$>> [Just True, Nothing, Just False]
-- [Just False,Nothing,Just True]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

-- | Flipped '(.)'
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f
{-# INLINE (.>) #-}

infixr 9 .>

-- | Alias for [].
type List = []

-- | Alias for (,).
type Tuple2 = (,)

-- | Alias for (,,).
type Tuple3 = (,,)
