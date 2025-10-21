{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Utils
  ( -- * Running
    ConfigIO (..),
    runConfigIO,
    NoConfigIO (..),
    runNoConfigIO,

    -- * Assertions
    makeConfigAndAssertEq,
    makeConfigAndAssertFieldEq,
    CompareField (..),
    (^=@),
    (^?=@),

    -- * Misc
    defaultConfig,
    notifySystemOSDBus,
    notifySystemOSNotifySend,
  )
where

import DBus.Client
  ( Client
      ( Client,
        clientInterfaces,
        clientObjects,
        clientPendingCalls,
        clientSignalHandlers,
        clientSocket,
        clientThreadID
      ),
  )
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( getHomeDirectory,
        getXdgDirectory
      ),
  )
import Effects.System.Terminal
  ( MonadTerminal (getChar, getTerminalSize),
    Window (Window),
  )
import Integration.Prelude as X
import Shrun.Configuration qualified as Config
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.Notify.System (NotifySystemMerged)
import Shrun.Configuration.Data.Notify.System qualified as Notify.System
import Shrun.Configuration.Env qualified as Env
import Shrun.Notify.DBus (MonadDBus (connectSession, notify))

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (ReaderT (IORef (List Text)) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadMask,
      MonadOptparse,
      MonadPathWriter,
      MonadIORef,
      MonadReader (IORef (List Text)),
      MonadSTM,
      MonadThrow
    )
    via (ReaderT (IORef (List Text))) IO

runConfigIO :: ConfigIO a -> IORef (List Text) -> IO a
runConfigIO (MkConfigIO rdr) = runReaderT rdr

-- HACK: Listing all the MonadPathReader methods is tedious and unnecessary,
-- so rather than list all of them like @foo = error "todo"@, we simply
-- disable the warning with -Wno-missing-methods

instance MonadPathReader ConfigIO where
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist

#if OSX
  getXdgDirectory _ _ =
    pure (unsafeEncode $ concatDirs ["test", "integration", "toml", "osx"])
#else
  getXdgDirectory _ _ =
    pure (unsafeEncode $ concatDirs ["test", "integration", "toml"])
#endif

instance MonadTerminal ConfigIO where
  putStr = error "putStr: unimplemented"

  -- capture logs
  putStrLn t = ask >>= (`modifyIORef'` (T.pack t :))

  getChar = error "getChar: unimplemented"

  -- hardcoded so we can test 'detect'
  getTerminalSize = pure (Window 23 87)

instance MonadDBus ConfigIO where
  connectSession =
    pure
      $ Client
        { clientSocket = error "todo",
          clientPendingCalls = error "todo",
          clientSignalHandlers = error "todo",
          clientObjects = error "todo",
          clientThreadID = error "todo",
          clientInterfaces = error "todo"
        }
  notify = error "notify: unimplemented"

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (ReaderT (IORef (List Text)) IO a)
  deriving
    ( Applicative,
      MonadPathWriter,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadMask,
      MonadOptparse,
      MonadSTM,
      MonadThrow
    )
    via (ReaderT (IORef (List Text))) IO
  deriving (MonadDBus) via ConfigIO

runNoConfigIO :: NoConfigIO a -> IORef (List Text) -> IO a
runNoConfigIO (MkNoConfigIO rdr) = runReaderT rdr

instance MonadPathReader NoConfigIO where
  getXdgDirectory _ _ = pure [osp|./|]
  getHomeDirectory = error "getHomeDirectory: unimplemented"
  doesFileExist = liftIO . doesFileExist

deriving via ConfigIO instance MonadTerminal NoConfigIO

-- | Makes a 'MergedConfig' for the given monad and compares the result with
-- the expectation.
makeConfigAndAssertEq ::
  forall m.
  ( MonadEnv m,
    MonadFileReader m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  -- | Expectation.
  MergedConfig ->
  PropertyT IO ()
makeConfigAndAssertEq args toIO expected = do
  result <- makeMergedConfig args toIO
  expected === result

-- | Used for testing a selection of MergedConfig's fields rather than the
-- entire structure.
data CompareField where
  -- | Tests a lens.
  MkCompareField :: (Eq a, Show a) => Lens' MergedConfig a -> a -> CompareField
  -- | Tests an affine traversal.
  MkCompareFieldMaybe ::
    (Eq a, Show a) =>
    AffineTraversal' MergedConfig a ->
    Maybe a ->
    CompareField

-- | Alias for 'MkCompareField'.
(^=@) :: (Eq a, Show a) => Lens' MergedConfig a -> a -> CompareField
l ^=@ r = MkCompareField l r

infix 1 ^=@

-- | Alias for 'MkCompareFieldMaybe'.
(^?=@) :: (Eq a, Show a) => AffineTraversal' MergedConfig a -> Maybe a -> CompareField
l ^?=@ r = MkCompareFieldMaybe l r

infix 1 ^?=@

-- | Like 'makeConfigAndAssertEq' except we only compare select fields.
makeConfigAndAssertFieldEq ::
  forall m.
  ( MonadEnv m,
    MonadFileReader m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  -- | List of expectations.
  List CompareField ->
  PropertyT IO ()
makeConfigAndAssertFieldEq args toIO comparisons = do
  result <- makeMergedConfig args toIO

  for_ comparisons $ \case
    MkCompareField l expected -> expected === result ^. l
    MkCompareFieldMaybe l expected -> expected === result ^? l

makeMergedConfig ::
  forall m.
  ( MonadEnv m,
    MonadFileReader m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  PropertyT IO MergedConfig
makeMergedConfig args toIO = do
  eResult <- tryMySync $ liftIO $ toIO $ withArgs args Env.getMergedConfig

  annotateShow args

  case eResult of
    Left ex -> do
      annotate $ displayException ex
      failure
    Right result -> do
      pure result

-- | Convenience for tests expecting a default config. The test should
-- pass a single command 'cmd'.
defaultConfig :: IO MergedConfig
defaultConfig = Config.mergeConfig args mempty
  where
    args = Args.defaultArgs (NESeq.singleton "cmd")

notifySystemOSDBus :: NotifySystemMerged
#if OSX
notifySystemOSDBus = Notify.System.AppleScript
#else
notifySystemOSDBus = Notify.System.DBus ()
#endif

notifySystemOSNotifySend :: NotifySystemMerged
#if OSX
notifySystemOSNotifySend = Notify.System.AppleScript
#else
notifySystemOSNotifySend = Notify.System.NotifySend
#endif
