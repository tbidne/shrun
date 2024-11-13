{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

{- ORMOLU_DISABLE -}

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
    EitherString (..),
    fromFoldable,
    onJust,
    (<<$>>),
    (<<&>>),
    convertIntegral,
    unsafeConvertIntegral,
    neToList,
    unsafeListToNE,
    setUncaughtExceptionHandlerDisplay,

    -- * 'Text' replacements for 'P.String' functions.
    showt,
    displayExceptiont,

#if !MIN_VERSION_base(4, 20, 0)

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,
    Tuple4,

#endif

    -- * Debug Utils
    todo,
    traceFile,
    traceFileLine,

    -- * Prelude exports
    module X,
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (*>), (<*>)),
    (<**>),
  )
import Control.Category as X (Category ((.)), (<<<), (>>>))
import Control.Concurrent as X (threadDelay)
import Control.Exception as X
  ( Exception (displayException, fromException),
    SomeException,
  )
import Control.Exception.Utils as X
  ( TextException,
    catchSync,
    exitFailure,
    trySync,
  )
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
import Control.Monad.Catch as X
  ( MonadCatch,
    MonadMask,
    MonadThrow,
    bracket,
    catch,
    finally,
    mask,
    throwM,
    try,
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
import Data.Bits (Bits, toIntegralSized)
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
import Data.Function as X (const, flip, id, ($), (&))
import Data.Functor as X
  ( Functor (fmap),
    ($>),
    (<$>),
    (<&>),
  )
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
#if MIN_VERSION_base(4, 20, 0)
import Data.List as X (List, filter, replicate, zip, (++))
#else
import Data.List as X (filter, replicate, zip, (++))
#endif
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
import Data.Traversable as X (Traversable (sequenceA, traverse), for)
import Data.Tuple as X (fst, snd)
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3, Tuple4)
#endif
import Data.List.NonEmpty qualified as NE
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
import FileSystem.OsPath as X (OsPath, decodeLenient, osp, (</>))
import FileSystem.OsPath qualified as OsPath
import FileSystem.UTF8 as X (decodeUtf8)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (toEnum))
import GHC.Err as X (error, undefined)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Real as X (Integral, truncate)
import GHC.Show as X (Show (show, showsPrec))
import GHC.Stack as X (HasCallStack)
import Optics.Core as X
  ( A_Getter,
    A_Lens,
    A_Setter,
    AffineTraversal',
    An_AffineFold,
    An_AffineTraversal,
    An_Iso,
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
    lensVL,
    over',
    preview,
    prism,
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
import System.Console.Regions as X (ConsoleRegion, RegionLayout (Linear))
import System.Exit as X (ExitCode (ExitFailure, ExitSuccess))
import System.IO as X (FilePath, Handle, IO, IOMode (AppendMode, WriteMode), print)
import System.IO.Unsafe (unsafePerformIO)
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
import Type.Reflection (Typeable)
import Type.Reflection qualified as Typeable
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
{-# INLINEABLE fromFoldable #-}

-- | Lifted fmap.
--
-- >>> not <<$>> [Just True, Nothing, Just False]
-- [Just False,Nothing,Just True]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

{-# INLINE (<<$>>) #-}

-- | Flipped '(<<$>>)'; lifted `(<&>)`.
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)
{-# INLINE (<<&>>) #-}

#if !MIN_VERSION_base(4, 20, 0)

-- | Alias for [].
type List = []

-- | Alias for (,).
type Tuple2 = (,)

-- | Alias for (,,).
type Tuple3 = (,,)

-- | Alias for (,,,).
type Tuple4 = (,,,)

#endif

neToList :: NonEmpty a -> List a
neToList = NE.toList

unsafeListToNE :: (HasCallStack) => List a -> NonEmpty a
unsafeListToNE = NE.fromList

-- | Like 'fromIntegral', except:
--
--   1. The conversion is only between integral types.
--   2. Errors rather than silently rounds for bounds issues.
unsafeConvertIntegral ::
  forall a b.
  ( Bits a,
    Bits b,
    HasCallStack,
    Integral a,
    Integral b,
    Show a,
    Typeable a,
    Typeable b
  ) =>
  a ->
  b
unsafeConvertIntegral x = case convertIntegral x of
  Right y -> y
  Left err -> error err

-- | Like 'fromIntegral', except the conversion is only between integral types.
convertIntegral ::
  forall a b.
  ( Bits a,
    Bits b,
    Integral a,
    Integral b,
    Show a,
    Typeable a,
    Typeable b
  ) =>
  a ->
  Either String b
convertIntegral x = case toIntegralSized x of
  Just y -> Right y
  Nothing ->
    Left $
      mconcat
        [ "Failed converting ",
          show x,
          " from ",
          show (Typeable.typeOf x),
          " to ",
          show $ Typeable.typeOf (undefined :: b)
        ]

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}

traceFile :: FilePath -> Text -> a -> a
traceFile path txt x = writeFn `seq` x
  where
    io = appendFileUtf8 (OsPath.unsafeEncode path) txt
    writeFn = unsafePerformIO io

traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

setUncaughtExceptionHandlerDisplay :: IO ()
setUncaughtExceptionHandlerDisplay =
  setUncaughtExceptionHandler printExceptExitCode
  where
    printExceptExitCode ex = case fromException ex of
      Just ExitSuccess -> pure ()
      -- for command failures
      Just (ExitFailure _) -> pure ()
      Nothing -> putStrLn $ displayException ex

onJust :: b -> Maybe a -> (a -> b) -> b
onJust x m f = maybe x f m

-- | Either, specializing Left to String, for the purposes of MonadFail.
data EitherString a
  = EitherLeft String
  | EitherRight a
  deriving stock (Eq, Functor, Show)

instance Applicative EitherString where
  pure = EitherRight

  EitherRight f <*> EitherRight x = EitherRight (f x)
  EitherLeft x <*> _ = EitherLeft x
  _ <*> EitherLeft x = EitherLeft x

instance Monad EitherString where
  EitherRight x >>= f = f x
  EitherLeft x >>= _ = EitherLeft x

instance Foldable EitherString where
  foldr _ e (EitherLeft _) = e
  foldr f e (EitherRight x) = f x e

instance Traversable EitherString where
  sequenceA (EitherLeft x) = pure (EitherLeft x)
  sequenceA (EitherRight x) = EitherRight <$> x

  traverse _ (EitherLeft x) = pure (EitherLeft x)
  traverse f (EitherRight x) = EitherRight <$> f x

instance MonadFail EitherString where
  fail = EitherLeft
