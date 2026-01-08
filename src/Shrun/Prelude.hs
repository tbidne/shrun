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

    -- * Optparse
    DocOA,

    -- * Pretty printing
    PrettySwitch (..),
    indentField,
    prettyBytesFloat,
    prettyBytesInt,
    prettyMaybe,
    prettyToText,

    -- * Misc utilities
    EitherString (..),
    fromFoldable,
    foldMapA,
    onJust,
    (<<$>>),
    (<<&>>),
    convertIntegral,
    unsafeConvertIntegral,
    catSeqMaybes,
    neToList,
    listToSeq,
    neseqToSeq,
    unsafeListToNE,
    unsafeListToNESeq,

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
    traceFileA,
    traceFileLine,
    traceFileLineA,

    -- * Exceptions
    TermException (..),
    tryMySync,
    onMyAsync,
    isMyAsync,
    isTermException,

    -- * Prelude exports
    module X,
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
    asum,
    (<**>),
  )
import Control.Category as X (Category ((.)), (<<<), (>>>))
import Control.Concurrent as X (threadDelay)
import Control.Concurrent.STM.TMVar as X (TMVar, newTMVar)
import Control.DeepSeq as X (NFData, force)
import Control.Exception as X
  ( Exception (displayException, fromException, toException),
    SomeException,
  )
import Control.Exception.Utils as X
  ( StringException (MkStringException),
    exitFailure,
    throwString,
    throwText,
  )
import Control.Exception.Utils qualified as Ex.Utils
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
    onException,
    throwM,
    try,
  )
import Control.Monad.Catch.Pure qualified as C
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.Trans as X (MonadTrans (lift))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Bits (Bits, toIntegralSized)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X
  ( Bytes (MkBytes),
    FloatingFormatter,
    Fromℤ,
    Size (B),
    Sized,
    _MkBytes,
  )
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (IntegralFormatter)
import Data.Bytes.Formatting qualified as BytesFmt
import Data.Bytes.Formatting.Base (BaseFormatter, Formatter)
import Data.Char as X (Char)
import Data.Coerce as X (coerce)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (fold, foldMap, foldl', foldr, toList),
    any,
    for_,
    length,
    traverse_,
  )
import Data.Foldable1 as X (Foldable1, fold1, foldr1)
import Data.Function as X (const, flip, id, ($), (&))
import Data.Functor as X
  ( Functor (fmap),
    ($>),
    (<$),
    (<$>),
    (<&>),
  )
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Hashable as X (Hashable (hashWithSalt))
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
#if MIN_VERSION_base(4, 20, 0)
import Data.List as X (List, filter, replicate, zip, (++))
#else
import Data.List as X (filter, replicate, zip, (++))
#endif
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Ap (Ap, getAp), Monoid (mconcat, mempty))
import Data.Ord as X (Ord ((<), (<=), (>), (>=)), Ordering)
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq ((:<|), (:|>)), pattern Empty)
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)), pattern IsEmpty)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Singletons (SingI)
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text, pack, unpack)
import Data.Text qualified as T
import Data.Traversable as X (Traversable (sequenceA, traverse), for)
import Data.Tuple as X (fst, snd, uncurry)
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3, Tuple4)
#endif
import Data.Type.Equality as X (type (~))
import Data.Void as X (Void, absurd)
import Effects.Concurrent.Async as X (MonadAsync)
import Effects.Concurrent.STM as X
  ( MonadSTM (atomically),
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
import Effects.Concurrent.Thread as X
  ( MVar,
    MonadMVar (newMVar, putMVar, tryTakeMVar),
    MonadThread,
    microsleep,
    sleep,
  )
import Effects.Evaluate as X (MonadEvaluate (evaluate))
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
    removeFileIfExists_,
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
import Effects.Optparse as X (MonadOptparse (customExecParser, execParser))
import Effects.System.Environment as X (MonadEnv (withArgs))
import Effects.System.Posix.Signals as X (MonadPosixSignals)
import Effects.System.Process as X (CreateProcess, MonadProcess)
import Effects.System.Terminal as X
  ( MonadTerminal,
    Window (Window),
    getTerminalSize,
    putStr,
    putStrLn,
    putText,
    putTextLn,
  )
import Effects.Time as X (MonadTime, withTiming)
import FileSystem.OsPath as X
  ( OsPath,
    decodeLenient,
    decodeThrowM,
    encodeThrowM,
    osp,
    ospPathSep,
    unsafeEncode,
    (</>),
  )
import FileSystem.OsPath qualified as OsPath
import FileSystem.UTF8 as X (decodeUtf8, decodeUtf8ThrowM)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (fromEnum, toEnum))
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
import GHC.Stack as X (HasCallStack, withFrozenCallStack)
import Numeric.Algebra (MGroup)
import Numeric.Algebra as X
  ( ASemigroup ((.+.)),
    MMonoid (one),
    MSemigroup,
    Normed,
  )
import Numeric.Convert.Integer as X
  ( FromInteger (fromZ),
    ToInteger (toZ),
    fromℤ,
    toℤ,
  )
import Numeric.Data.NonNegative as X
  ( NonNegative (MkNonNegative),
    mkNonNegative,
    unsafeNonNegative,
  )
import Numeric.Data.Positive as X
  ( Positive (MkPositive),
    mkPositive,
    unsafePositive,
  )
import Optics.Core as X
  ( A_Getter,
    A_Lens,
    A_Prism,
    A_Setter,
    AffineFold,
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
    Optic,
    Optic',
    Prism,
    Prism',
    afolding,
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
import Options.Applicative.Help qualified as H
import Prettyprinter as X
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    indent,
    nest,
    punctuate,
    vcat,
  )
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as PrettyprinterT
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
import Text.Printf (PrintfArg)
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

neseqToSeq :: NESeq a -> Seq a
neseqToSeq = NESeq.toSeq

unsafeListToNE :: (HasCallStack) => List a -> NonEmpty a
unsafeListToNE = NE.fromList

unsafeListToNESeq :: (HasCallStack) => List a -> NESeq a
unsafeListToNESeq = NESeq.fromList . NE.fromList

listToSeq :: List a -> Seq a
listToSeq = Seq.fromList

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

traceFileA :: (Applicative f) => FilePath -> Text -> f ()
traceFileA f t = traceFile f t (pure ())

traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

traceFileLineA :: (Applicative f) => FilePath -> Text -> f ()
traceFileLineA f t = traceFileLine f t (pure ())

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

-- | TermException is explicitly for when the current process is cancelled
-- (SIGTERM on posix). We use a separate type so that we can distinguish it
-- from potentially other ThreadKilleds that might be sent (e.g. bugs).
data TermException = MkTermException
  deriving stock (Show)

instance Exception TermException where
  displayException _ = "Received terminated signal"

-- We want our TermException to be considered Async for the purposes of our
-- handlers since it morally is (spawned by an outside signal). Hence
-- these should be used over the usual trySync, thus always in scope.
tryMySync :: (HasCallStack, MonadCatch m) => m a -> m (Either SomeException a)
tryMySync = Ex.Utils.tryIf (not . isMyAsync)

onMyAsync :: (HasCallStack, MonadCatch m) => m a -> m b -> m a
onMyAsync action handler = withFrozenCallStack catchAsync action $ \e -> do
  void $ C.try @_ @SomeException handler
  throwM e
  where
    catchAsync = Ex.Utils.catchIf @_ @SomeException isMyAsync

isMyAsync :: (Exception e) => e -> Bool
isMyAsync e = Ex.Utils.isAsyncException e || isTermException e

isTermException :: (Exception e) => e -> Bool
isTermException e = case fromException (toException e) of
  Just MkTermException -> True
  Nothing -> False

foldMapA :: (Applicative m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapA f = getAp <$> foldMap (Ap . f)

catSeqMaybes :: Seq (Maybe a) -> Seq a
catSeqMaybes = foldl' go Empty
  where
    go acc Nothing = acc
    go acc (Just x) = acc :|> x

-- | Alias for optparse's Doc, so we do not clash with prettyprinter's Doc.
-- The former is an alias for the latter's Doc AnsiStyle.
type DocOA = H.Doc

newtype PrettySwitch = MkPrettySwitch Bool

instance Pretty PrettySwitch where
  pretty = \case
    MkPrettySwitch True -> "on"
    MkPrettySwitch False -> "off"

indentField :: Doc ann -> Doc ann
indentField = indent 2

prettyBytesFloat ::
  ( BaseFormatter a ~ FloatingFormatter,
    Fromℤ a,
    MGroup a,
    Normed a,
    Ord a,
    PrintfArg a,
    SingI s
  ) =>
  Bytes s a ->
  Doc ann
prettyBytesFloat = prettyBytes (BytesFmt.MkFloatingFormatter (Just 2))

prettyBytesInt ::
  ( BaseFormatter a ~ IntegralFormatter,
    Fromℤ a,
    MGroup a,
    Normed a,
    Ord a,
    PrintfArg a,
    SingI s
  ) =>
  Bytes s a ->
  Doc ann
prettyBytesInt = prettyBytes BytesFmt.MkIntegralFormatter

prettyBytes ::
  ( BaseFormatter a ~ fmt,
    Fromℤ a,
    Formatter fmt,
    MGroup a,
    Normed a,
    Ord a,
    PrintfArg a,
    SingI s
  ) =>
  fmt ->
  Bytes s a ->
  Doc ann
prettyBytes fmt =
  pretty
    . BytesFmt.formatSized fmt BytesFmt.sizedFormatterNatural
    . Bytes.normalize

prettyMaybe :: (Pretty a) => Maybe a -> Doc ann
prettyMaybe Nothing = "off"
prettyMaybe (Just x) = pretty x

prettyToText :: (Pretty a) => a -> Text
prettyToText =
  PrettyprinterT.renderStrict
    . Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
    . pretty
