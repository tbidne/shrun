{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude.FuncEnv
  ( -- * Potential IO wrapper
    unConfigIO,
    ConfigIOEnv (..),

    -- * Shrun environment for functional tests
    FuncEnv (..),
    runRegionLoggerFuncEnv,
    runNotifyFuncEnv,
  )
where

import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( DoesFileExist,
        DoesPathExist,
        GetCurrentDirectory,
        GetFileSize,
        GetXdgDirectory
      ),
    XdgDirectory,
  )
import Effectful.Notify.Dynamic (Notify (InitNotifyEnv, Notify))
import Effectful.Terminal.Dynamic (Terminal (GetTerminalSize, PutStr, PutStrLn))
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
import Shrun.Logging.RegionLogger
  ( RegionLogger
      ( DisplayRegions,
        LogGlobal,
        LogRegion,
        RegionList,
        WithRegion
      ),
  )
import Shrun.Prelude

-- | Enviroment used by 'ConfigIO'. For when we want some IO behavior mocked.
data ConfigIOEnv = MkConfigIOEnv
  { cwdDir :: Maybe OsPath,
    logs :: IORef (List Text),
    xdgDir :: Maybe (XdgDirectory -> OsPath)
  }

makeFieldLabelsNoPrefix ''ConfigIOEnv

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
type ConfigEffects =
  [ Concurrent,
    Environment,
    FileReader,
    FileWriter,
    HandleReader,
    HandleWriter,
    Notify (),
    Optparse,
    PathReader,
    PathWriter,
    PosixFiles,
    Process,
    Terminal,
    Time,
    Prim,
    Reader ConfigIOEnv,
    IOE
  ]

-- Note that we have two notification effects: One in ConfigEffects
-- (runNotifyConfigIO), and one that is used while running Shrun with
-- FuncEnv (runNotifyFuncEnv). Both stages require a notification effect,
-- so while we /could/ supply a single one in the "outer" config stage,
-- the ConfigIOEnv does not have the notifications data on it.
--
-- IOW, we'd have to unify the notifications data in order to use a single
-- effect. Doable, but for now the easiest thing to do is just have two
-- handlers, knowing that the config handler just throws away data,
-- and isn't really used anyway (all it does is "init").

unConfigIO ::
  Eff ConfigEffects a ->
  Eff [Reader ConfigIOEnv, IOE] a
unConfigIO =
  runPrim
    . runTime
    . runTerminalConfigIO
    . runProcess
    . runPosixFiles
    . runPathWriter
    . runPathReaderConfigIO
    . runOptparse
    . runNotifyConfigIO
    . runHandleWriter
    . runHandleReader
    . runFileWriter
    . runFileReader
    . runEnvironment
    . runConcurrent

runNotifyConfigIO ::
  () =>
  Eff (Notify () : es) a ->
  Eff es a
runNotifyConfigIO = interpret_ $ \case
  InitNotifyEnv _ -> pure ()
  Notify _ _ -> pure ()

runPathReaderConfigIO ::
  ( IOE :> es,
    Reader ConfigIOEnv :> es
  ) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderConfigIO = reinterpret_ runPathReader $ \case
  DoesFileExist p -> doesFileExist p
  DoesPathExist p -> doesPathExist p
  GetCurrentDirectory -> do
    mCwd <- asks @ConfigIOEnv (view #cwdDir)
    case mCwd of
      Nothing -> getCurrentDirectory
      Just cwd -> pure cwd
  GetFileSize p -> getFileSize p
  GetXdgDirectory xdg p -> do
    mOnXdg <- asks @ConfigIOEnv (view #xdgDir)
    case mOnXdg of
      Nothing -> getXdgDirectory xdg p
      Just onXdg -> pure $ onXdg xdg </> p
  other -> error $ "runPathReaderConfigIO: " ++ showEffectCons other

runTerminalConfigIO ::
  ( Prim :> es,
    Reader ConfigIOEnv :> es
  ) =>
  Eff (Terminal : es) a ->
  Eff es a
runTerminalConfigIO = interpret_ $ \case
  PutStr s -> do
    logsRef <- asks @ConfigIOEnv (view #logs)
    modifyIORef logsRef (pack s :)
  PutStrLn s -> do
    logsRef <- asks @ConfigIOEnv (view #logs)
    modifyIORef logsRef (pack s :)
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
  GetTerminalSize -> pure $ Window 100 150
  other -> error $ "runTerminalConfigIO: " ++ showEffectCons other

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

data FuncEnv = MkFuncEnv
  { coreEnv :: Env () (),
    logs :: IORef (List Text),
    shrunNotes :: IORef (List Note)
  }

makeFieldLabelsNoPrefix ''FuncEnv

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

instance HasNotifyConfig FuncEnv () where
  getNotifyConfig = getNotifyConfig . view #coreEnv

runRegionLoggerFuncEnv ::
  ( Concurrent :> es,
    Prim :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (RegionLogger () : es) a ->
  Eff es a
runRegionLoggerFuncEnv = interpret $ \env -> \case
  LogGlobal txt -> do
    ls <- asks @FuncEnv $ view #logs
    modifyIORef ls (txt :)
  LogRegion _ _ txt -> do
    ls <- asks @FuncEnv $ view #logs
    modifyIORef ls (txt :)
  WithRegion _layout regionToShell -> localSeqUnlift env $ \unlift ->
    unlift (regionToShell ())
  DisplayRegions m -> localSeqUnlift env $ \unlift -> unlift m
  RegionList -> atomically $ newTMVar []

runNotifyFuncEnv ::
  ( Prim :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (Notify () : es) a ->
  Eff es a
runNotifyFuncEnv = interpret_ $ \case
  InitNotifyEnv _ -> pure ()
  Notify _ note -> do
    notesRef <- asks @FuncEnv (view #shrunNotes)
    modifyIORef notesRef (note :)
