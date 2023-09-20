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
import Data.Bifunctor as X (Bifunctor)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
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
import Effectful as X (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent as X (Concurrent, runConcurrent)
import Effectful.Concurrent.STM.TBQueue.Static as X
  ( TBQueue,
    flushTBQueueA,
    newTBQueueA,
    readTBQueueA,
    writeTBQueueA,
  )
import Effectful.Concurrent.STM.TVar.Static as X
  ( TVar,
    modifyTVarA',
    newTVarA,
    readTVarA,
    writeTVarA,
  )
import Effectful.Exception as X
  ( Exception (displayException, fromException),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    bracket,
    catch,
    catchAny,
    displayException,
    exitFailure,
    finally,
    mask,
    throwM,
    try,
    tryAny,
  )
import Effectful.FileSystem.FileReader.Static as X (FileReaderStatic, readFileUtf8Lenient, readFileUtf8ThrowM, runFileReaderStaticIO)
import Effectful.FileSystem.FileWriter.Static as X
  ( FileWriterStatic,
    appendFileUtf8,
    writeFileUtf8,
  )
import Effectful.FileSystem.HandleReader.Static as X (HandleReaderStatic)
import Effectful.FileSystem.HandleWriter.Static as X
  ( HandleWriterStatic,
    hClose,
    hFlush,
    hPutUtf8,
    openBinaryFile,
  )
import Effectful.FileSystem.PathReader.Dynamic as X
  ( PathReaderDynamic,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getXdgConfig,
    getXdgState,
  )
import Effectful.FileSystem.PathWriter.Static as X
  ( PathWriterStatic,
    removeDirectoryIfExists,
    removeFile,
    removeFileIfExists,
  )
import Effectful.FileSystem.Utils as X (OsPath, decodeUtf8Lenient, osp, (</>))
import Effectful.IORef.Static as X
  ( IORef,
    IORefStatic,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
    runIORefStaticIO,
    writeIORef,
  )
import Effectful.Optparse.Static as X (OptparseStatic, execParser, runOptparseStaticIO)
import Effectful.Process.Typed as X
  ( Process,
    TypedProcess,
  )
import Effectful.Reader.Static as X (Reader, asks, runReader)
import Effectful.Terminal.Dynamic as X
  ( TerminalDynamic,
    putStr,
    putStrLn,
    putText,
    putTextLn,
  )
import Effectful.Time.Dynamic as X (TimeDynamic)
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
    AffineTraversal',
    An_AffineFold,
    An_AffineTraversal,
    Getter,
    Is,
    Iso',
    LabelOptic (labelOptic),
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
headMaybe :: List a -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

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
