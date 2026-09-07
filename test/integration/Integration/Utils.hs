{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Utils
  ( -- * Running
    runConfigIO,
    runNoConfigIO,

    -- * Assertions
    makeConfigAndAssertEq,
    makeConfigAndAssertFieldEq,
    CompareField (..),
    (^=@),
    (^?=@),

    -- * Misc
    defaultConfig,
    notifySystemDBus,
    notifySystemNotifySend,

    -- * Effects
    IntEffects,
    runFileWriterConfig,
    runNotifyConfig,
    runPathReaderConfig,
    runPathWriterConfig,
  )
where

import Data.Text qualified as T
import Effectful.FileSystem.FileWriter.Dynamic (FileWriter (WriteBinaryFile))
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( DoesDirectoryExist,
        DoesFileExist,
        DoesPathExist,
        GetCurrentDirectory,
        GetFileSize,
        GetXdgDirectory
      ),
  )
import Effectful.FileSystem.PathWriter.Dynamic (PathWriter (CreateDirectoryIfMissing, RemoveFile))
import Effectful.Notify.Dynamic (Notify (InitNotifyEnv))
import Effectful.Notify.Dynamic qualified as Notify
import Effectful.Terminal.Dynamic
  ( Terminal (GetTerminalSize, PutStrLn),
  )
import Integration.Prelude as X
import Shrun.Configuration qualified as Config
import Shrun.Configuration.Args qualified as Args
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Env qualified as Env
import System.OsPath qualified as OsP

type IntEffects :: List Effect
type IntEffects =
  [ Concurrent,
    Environment,
    FileReader,
    FileWriter,
    HandleWriter,
    Notify (),
    Optparse,
    PathReader,
    PathWriter,
    PosixFiles,
    Terminal,
    Prim,
    Reader (IORef (List Text)),
    IOE
  ]

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
runConfigIO ::
  Eff IntEffects a ->
  IORef (List Text) ->
  IO a
runConfigIO m ref =
  runEff
    . runReader ref
    . runPrim
    . runTerminalConfig
    . runPosixFiles
    . runPathWriterConfig
    . runPathReaderConfig
    . runOptparse
    . runNotifyConfig
    . runHandleWriter
    . runFileWriterConfig
    . runFileReader
    . runEnvironment
    . runConcurrent
    $ m

runNotifyConfig :: Eff (Notify () : es) a -> Eff es a
runNotifyConfig = interpret_ $ \case
  InitNotifyEnv _ -> pure ()
  other -> error $ "runNotifyConfig: " ++ showEffectCons other

runFileWriterConfig :: Eff (FileWriter : es) a -> Eff es a
runFileWriterConfig = interpret_ $ \case
  WriteBinaryFile {} -> pure ()
  other -> error $ "runFileWriterConfig: " ++ showEffectCons other

runPathReaderConfig :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReaderConfig = reinterpret_ runPathReader $ \case
  GetCurrentDirectory -> getCurrentDirectory
  GetFileSize p -> getFileSize p
  DoesFileExist p -> doesFileExistIgnoreLocalShrun p
  DoesDirectoryExist p -> doesDirectoryExist p
  DoesPathExist p -> doesFileExistIgnoreLocalShrun p
  GetXdgDirectory {} -> pure xdgDirPathOS
  other -> error $ "runPathReaderConfig: " ++ showEffectCons other

runPathWriterConfig :: (IOE :> es) => Eff (PathWriter : es) a -> Eff es a
runPathWriterConfig = reinterpret_ runPathWriter $ \case
  CreateDirectoryIfMissing {} -> pure ()
  -- Paranoid, only delete files we know about for the tests.
  RemoveFile p -> do
    when ("large-file-del" `T.isInfixOf` pTxt) $ do
      removeFile p
    where
      pTxt = pack $ decodeLenient p
  other -> error $ "runPathWriterConfig: " ++ showEffectCons other

runTerminalConfig :: (Reader (IORef [Text]) :> es, Prim :> es) => Eff (Terminal : es) a -> Eff es a
runTerminalConfig = interpret_ $ \case
  PutStrLn t -> ask >>= (`modifyIORef` (T.pack t :))
  -- hardcoded so we can test 'detect'
  GetTerminalSize -> pure (Window 23 87)
  other -> error $ "runTerminalConfig: " ++ showEffectCons other

runPathReaderNoConfig :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReaderNoConfig = reinterpret_ runPathReader $ \case
  GetCurrentDirectory -> getCurrentDirectory
  DoesFileExist p -> doesFileExistIgnoreLocalShrun p
  DoesPathExist p -> doesFileExistIgnoreLocalShrun p
  GetXdgDirectory {} -> pure [osp|./|]
  other -> error $ "runPathReaderNoConfig: " ++ showEffectCons other

runPathWriterNoConfig :: Eff (PathWriter : es) a -> Eff es a
runPathWriterNoConfig = interpret_ $ \case
  CreateDirectoryIfMissing {} -> pure ()
  other -> error $ "runPathWriterNoConfig: " ++ showEffectCons other

runNoConfigIO ::
  Eff IntEffects a ->
  IORef (List Text) ->
  IO a
runNoConfigIO m ref =
  runEff
    . runReader ref
    . runPrim
    . runTerminalConfig
    . runPosixFiles
    . runPathWriterNoConfig
    . runPathReaderNoConfig
    . runOptparse
    . runNotifyConfig
    . runHandleWriter
    . runFileWriterConfig
    . runFileReader
    . runEnvironment
    . runConcurrent
    $ m

-- | Makes a 'MergedConfig' for the given monad and compares the result with
-- the expectation.
makeConfigAndAssertEq ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    FileWriter :> es,
    Prim :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from Eff to IO.
  (forall x. Eff es x -> IO x) ->
  -- | Expectation.
  MergedConfig NotifyEnv ->
  PropertyT IO ()
makeConfigAndAssertEq args toIO expected = do
  result <- makeMergedConfig args toIO
  expected === result

-- | Used for testing a selection of MergedConfig's fields rather than the
-- entire structure.
data CompareField where
  -- | Tests a lens.
  MkCompareField :: (Eq a, Show a) => Lens' (MergedConfig NotifyEnv) a -> a -> CompareField
  -- | Tests an affine traversal.
  MkCompareFieldMaybe ::
    (Eq a, Show a) =>
    AffineTraversal' (MergedConfig NotifyEnv) a ->
    Maybe a ->
    CompareField

-- | Alias for 'MkCompareField'.
(^=@) :: (Eq a, Show a) => Lens' (MergedConfig NotifyEnv) a -> a -> CompareField
l ^=@ r = MkCompareField l r

infix 1 ^=@

-- | Alias for 'MkCompareFieldMaybe'.
(^?=@) :: (Eq a, Show a) => AffineTraversal' (MergedConfig NotifyEnv) a -> Maybe a -> CompareField
l ^?=@ r = MkCompareFieldMaybe l r

infix 1 ^?=@

-- | Like 'makeConfigAndAssertEq' except we only compare select fields.
makeConfigAndAssertFieldEq ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    FileWriter :> es,
    Prim :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  -- | List of expectations.
  List CompareField ->
  PropertyT IO ()
makeConfigAndAssertFieldEq args toIO comparisons = do
  result <- makeMergedConfig args toIO

  for_ comparisons $ \case
    MkCompareField l expected -> expected === result ^. l
    MkCompareFieldMaybe l expected -> expected === result ^? l

makeMergedConfig ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    FileWriter :> es,
    Prim :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  PropertyT IO (MergedConfig NotifyEnv)
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
defaultConfig :: (MonadIO m) => m (MergedConfig NotifyEnv)
defaultConfig = liftIO $ runDefaultIO $ Config.mergeConfig args mempty mempty
  where
    args = Args.defaultArgs ["cmd"]

runDefaultIO ::
  Eff [Terminal, Reader (IORef [Text]), Prim, IOE] a ->
  IO a
runDefaultIO m = do
  ref <- iorefIO $ newIORef []
  runEff
    . runPrim
    . runReader ref
    -- Essentially, derive MonadTerminal from NoConfigIO. This ensures we have the
    -- same windows size, which matters because 'detect' is the default line
    -- trunc.
    . runTerminalConfig
    $ m

notifySystemDBus :: NotifySystem
#if OSX
notifySystemDBus = Notify.NotifySystemAppleScript
#else
notifySystemDBus = Notify.NotifySystemDBus
#endif

notifySystemNotifySend :: NotifySystem
#if OSX
notifySystemNotifySend = Notify.NotifySystemAppleScript
#else
notifySystemNotifySend = Notify.NotifySystemNotifySend
#endif

-- Ignore these so that local files do not interfere with tests.
doesFileExistIgnoreLocalShrun :: (HasCallStack, PathReader :> es) => OsPath -> Eff es Bool
doesFileExistIgnoreLocalShrun p
  | pName == [osp|shrun.toml|] = pure False
  | pName == [osp|.shrun.toml|] = pure False
  | otherwise = doesFileExist p
  where
    pName = OsP.takeFileName p
