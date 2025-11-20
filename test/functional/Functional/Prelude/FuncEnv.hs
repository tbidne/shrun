{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude.FuncEnv
  ( -- * Potential IO wrapper
    FuncIO (..),
    unFuncIO,
    FuncIOEnv (..),

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
    HasCommands (getCleanup, getCommandDepGraph, getCommandStatus),
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

-- | Enviroment used by 'FuncIO'. For when we want some IO behavior mocked.
data FuncIOEnv = MkFuncIOEnv
  { cwdDir :: Maybe OsPath,
    xdgDir :: Maybe (XdgDirectory -> OsPath)
  }

instance
  ( k ~ A_Lens,
    a ~ Maybe OsPath,
    b ~ Maybe OsPath
  ) =>
  LabelOptic "cwdDir" k FuncIOEnv FuncIOEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncIOEnv a1 a2) ->
        fmap
          (\b -> MkFuncIOEnv b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Maybe (XdgDirectory -> OsPath),
    b ~ Maybe (XdgDirectory -> OsPath)
  ) =>
  LabelOptic "xdgDir" k FuncIOEnv FuncIOEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncIOEnv a1 a2) ->
        fmap
          (\b -> MkFuncIOEnv a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | In a real run, we run shrun with 'ShellT (Env IO) IO'. In our functional
-- tests, this is generally 'ShellT FuncEnv IO', which is mostly unmocked,
-- appart from things like terminal output and notifications.
--
-- However, we sometimes want to mock other parts, like the XDG directory.
-- FuncIO exists for this purpose. With this type, the runner is ultimately
-- 'ShellT (Env IO) FuncIO'.
--
-- Why do we not add the conditional XDG mocking to FuncEnv instead? Because
-- FuncEnv is only created _after_ shrun's configuration steps are run. But
-- we want XDG mocked for the configuration step itself.
--
-- This is generally IO, so FuncIO exists so we can instead inject FuncIO,
-- and use its instances.
newtype FuncIO a = MkFuncIO (ReaderT FuncIOEnv IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadDBus,
      MonadCatch,
      MonadEnv,
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
      MonadReader FuncIOEnv,
      MonadSTM,
      MonadTerminal,
      MonadThread,
      MonadTime,
      MonadThrow
    )

unFuncIO :: FuncIO a -> ReaderT FuncIOEnv IO a
unFuncIO (MkFuncIO rdr) = rdr

instance MonadPathReader FuncIO where
  doesFileExist = liftIO . doesFileExist

  getCurrentDirectory = do
    mCwd <- asks (view #cwdDir)
    case mCwd of
      Nothing -> liftIO getCurrentDirectory
      Just cwd -> pure cwd

  getXdgDirectory xdg p = do
    mOnXdg <- asks (view #xdgDir)
    case mOnXdg of
      Nothing -> liftIO (getXdgDirectory xdg p)
      Just onXdg -> pure $ onXdg xdg </> p

instance MonadPosixSignals FuncIO where
  installHandler s h m = MkFuncIO $ do
    hFromM <$> installHandler s (hToM h) m
    where
      hFromM = Signals.mapHandler MkFuncIO
      hToM = Signals.mapHandler unFuncIO

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

data FuncEnv = MkFuncEnv
  { coreEnv :: Env (),
    funcIOEnv :: FuncIOEnv,
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
      $ \f (MkFuncEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkFuncEnv b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ FuncIOEnv,
    b ~ FuncIOEnv
  ) =>
  LabelOptic "funcIOEnv" k FuncEnv FuncEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkFuncEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkFuncEnv a1 b a3 a4)
          (f a2)
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
      $ \f (MkFuncEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkFuncEnv a1 a2 b a4)
          (f a3)
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
      $ \f (MkFuncEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkFuncEnv a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

instance HasTimeout FuncEnv where
  getTimeout = getTimeout . view #coreEnv
  getHasTimedOut = getHasTimedOut . view #coreEnv

instance HasInit FuncEnv where
  getInit = getInit . view #coreEnv

instance HasCommands FuncEnv where
  getCleanup = getCleanup . view #coreEnv
  getCommandDepGraph = getCommandDepGraph . view #coreEnv
  getCommandStatus = getCommandStatus . view #coreEnv

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
