{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude.FuncEnv
  ( -- * Potential IO wrapper
    ConfigIO (..),
    unConfigIO,
    ConfigIOEnv (..),

    -- * Shrun environment for functional tests
    FuncEnv (..),
  )
where

import Effects.FileSystem.PathReader
  ( MonadPathReader (getCurrentDirectory, getXdgDirectory),
    XdgDirectory,
  )
import Effects.System.Posix.Signals (MonadPosixSignals (installHandler))
import Effects.System.Posix.Signals qualified as Signals
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCommandLogging (getCommandLogging),
    HasCommands (getCleanup, getCommandDepGraph, getCommandStatusMap),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getHasTimedOut, getTimeout),
  )
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        displayRegions,
        logGlobal,
        logRegion,
        regionList,
        withRegion
      ),
  )
import Shrun.Notify.DBus (MonadDBus)
import Shrun.Notify.MonadNotify (MonadNotify (notify), ShrunNote)
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | Enviroment used by 'ConfigIO'. For when we want some IO behavior mocked.
data ConfigIOEnv = MkConfigIOEnv
  { cwdDir :: Maybe OsPath,
    logs :: IORef (List Text),
    xdgDir :: Maybe (XdgDirectory -> OsPath)
  }

instance
  ( k ~ A_Lens,
    a ~ Maybe OsPath,
    b ~ Maybe OsPath
  ) =>
  LabelOptic "cwdDir" k ConfigIOEnv ConfigIOEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfigIOEnv a1 a2 a3) ->
        fmap
          (\b -> MkConfigIOEnv b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (List Text),
    b ~ IORef (List Text)
  ) =>
  LabelOptic "logs" k ConfigIOEnv ConfigIOEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfigIOEnv a1 a2 a3) ->
        fmap
          (\b -> MkConfigIOEnv a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Maybe (XdgDirectory -> OsPath),
    b ~ Maybe (XdgDirectory -> OsPath)
  ) =>
  LabelOptic "xdgDir" k ConfigIOEnv ConfigIOEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfigIOEnv a1 a2 a3) ->
        fmap
          (\b -> MkConfigIOEnv a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

-- | In a real run, we run shrun with 'ShellT (Env IO) IO'. In our functional
-- tests, this would generally be 'ShellT FuncEnv IO', which is mostly unmocked,
-- appart from things like terminal output and notifications.
--
-- However, we sometimes want to mock other parts, like the XDG directory.
-- ConfigIO exists for this purpose. With this type, the runner is ultimately
-- 'ShellT (Env IO) ConfigIO'.
--
-- Why do we not add the conditional XDG mocking to FuncEnv instead? Because
-- FuncEnv is only created _after_ shrun's configuration steps are run. But
-- we want XDG mocked for the configuration step itself.
--
-- In other words, our functional tests are running 'ShellT FuncEnv ConfigIO',
-- where 'ShellT FuncEnv' is what is actually run for the test logic
-- (mocking e.g. terminal output and notifications), whereas 'ConfigIO' is
-- very nearly pure IO, but mocks some things we occasionally want at the
-- __config__ stage (e.g. xdg, terminal size detection).
--
-- This is generally IO, so ConfigIO exists so we can instead inject ConfigIO,
-- and use its instances.
newtype ConfigIO a = MkConfigIO (ReaderT ConfigIOEnv IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadDBus,
      MonadCatch,
      MonadEnv,
      MonadEvaluate,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadMVar,
      MonadOptparse,
      MonadPathWriter,
      MonadProcess,
      MonadReader ConfigIOEnv,
      MonadSTM,
      MonadThread,
      MonadTime,
      MonadThrow
    )

unConfigIO :: ConfigIO a -> ReaderT ConfigIOEnv IO a
unConfigIO (MkConfigIO rdr) = rdr

instance MonadPathReader ConfigIO where
  doesFileExist = liftIO . doesFileExist

  getCurrentDirectory = do
    mCwd <- asks (view #cwdDir)
    case mCwd of
      Nothing -> liftIO getCurrentDirectory
      Just cwd -> pure cwd

  getFileSize = liftIO . getFileSize

  getXdgDirectory xdg p = do
    mOnXdg <- asks (view #xdgDir)
    case mOnXdg of
      Nothing -> liftIO (getXdgDirectory xdg p)
      Just onXdg -> pure $ onXdg xdg </> p

instance MonadPosixSignals ConfigIO where
  installHandler s h m = MkConfigIO $ do
    hFromM <$> installHandler s (hToM h) m
    where
      hFromM = Signals.mapHandler MkConfigIO
      hToM = Signals.mapHandler unConfigIO

instance MonadTerminal ConfigIO where
  putStr s = do
    logsRef <- asks (view #logs)
    modifyIORef' logsRef (pack s :)
  putStrLn = putStr . (<> "\n")

  -- Give this a large width so that test logs do not get cut off. We want
  -- to mock this anyway, as we do not want real, non-deterministic detection
  -- to be used in the tests, but there is an even greater urgency for mocking
  -- this: getTerminalSize actually fails in some instances!
  --
  -- In particular, if we run the test suite with shrun i.e.
  --
  --   TEST_FUNCTIONAL=1 shrun "cabal test functional"
  --
  -- Then getTerminalSize actually fails. Sadly we do not get any information
  -- why; ultimately, the reason is that our upstream dependency terminal-size
  -- returns Nothing. It seems the getTerminalSize call, when run via
  -- process, does not work.
  getTerminalSize = pure $ Window 100 150

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

data FuncEnv = MkFuncEnv
  { coreEnv :: Env (),
    logs :: IORef (List Text),
    shrunNotes :: IORef (List ShrunNote)
  }

instance
  ( k ~ A_Lens,
    a ~ Env (),
    b ~ Env ()
  ) =>
  LabelOptic "coreEnv" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (List Text),
    b ~ IORef (List Text)
  ) =>
  LabelOptic "logs" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (List ShrunNote),
    b ~ IORef (List ShrunNote)
  ) =>
  LabelOptic "shrunNotes" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3) ->
        fmap
          (\b -> MkFuncEnv a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance HasTimeout FuncEnv where
  getTimeout = getTimeout . view #coreEnv
  getHasTimedOut = getHasTimedOut . view #coreEnv

instance HasInit FuncEnv where
  getInit = getInit . view #coreEnv

instance HasCommands FuncEnv where
  getCleanup = getCleanup . view #coreEnv
  getCommandDepGraph = getCommandDepGraph . view #coreEnv
  getCommandStatusMap = getCommandStatusMap . view #coreEnv

instance HasAnyError FuncEnv where
  getAnyError = getAnyError . view #coreEnv

instance HasCommandLogging FuncEnv where
  getCommandLogging = getCommandLogging . view #coreEnv

instance HasCommonLogging FuncEnv where
  getCommonLogging = getCommonLogging . view #coreEnv

instance HasConsoleLogging FuncEnv () where
  getConsoleLogging = getConsoleLogging . view #coreEnv

instance HasFileLogging FuncEnv where
  getFileLogging = getFileLogging . view #coreEnv

instance HasNotifyConfig FuncEnv where
  getNotifyConfig = getNotifyConfig . view #coreEnv

instance (MonadIO m) => MonadRegionLogger (ShellT FuncEnv m) where
  type Region (ShellT FuncEnv m) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

  regionList = liftIO $ atomically $ newTMVar []

instance (MonadIO m) => MonadNotify (ShellT FuncEnv m) where
  notify note = do
    notesRef <- asks (view #shrunNotes)
    liftIO $ modifyIORef' notesRef (note :)
    pure Nothing
