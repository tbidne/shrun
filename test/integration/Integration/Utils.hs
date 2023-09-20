{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Utils
  ( runConfigIO,
    runNoConfigIO,
    SimpleEnv (..),
    makeEnvAndVerify,
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
import Data.Maybe (isJust)
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic
      ( DoesDirectoryExist,
        DoesFileExist,
        GetFileSize,
        GetXdgDirectory
      ),
  )
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.FileSystem.Utils qualified as FsUtils
import Effectful.Reader.Static (ask)
import Effectful.Terminal.Dynamic (TerminalDynamic (..))
import Effectful.Terminal.Dynamic qualified as Term
import Integration.Prelude as X
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( Env,
    KeyHide,
    StripControl,
    TruncRegion (TCmdLine, TCmdName),
    Truncation,
  )
import Shrun.Data.Command (CommandP1)
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Notify.DBus (DBusDynamic)
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.Types
  ( NotifyAction,
    NotifySystem (AppleScript, DBus, NotifySend),
    NotifySystemP1,
    NotifyTimeout,
  )

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
runConfigIO ::
  Eff
    [ DBusDynamic,
      TerminalDynamic,
      PathWriterStatic,
      PathReaderDynamic,
      OptparseStatic,
      HandleWriterStatic,
      FileWriterStatic,
      FileReaderStatic,
      IORefStatic,
      Reader (IORef [Text]),
      Environment,
      Concurrent,
      IOE
    ]
    a ->
  IORef [Text] ->
  IO a
runConfigIO m ref = run m
  where
    run =
      runEff
        . runConcurrent
        . Env.runEnvironment
        . runReader ref
        . runIORefStaticIO
        . FR.runFileReaderStaticIO
        . FW.runFileWriterStaticIO
        . HW.runHandleWriterStaticIO
        . runOptparseStaticIO
        . runPathReaderConfigIO
        . PW.runPathWriterStaticIO
        . runTerminalConfigIO
        . runDBusConfigIO

{- ORMOLU_DISABLE -}

runPathReaderConfigIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReaderConfigIO = reinterpret PR.runPathReaderStaticIO $ \_ -> \case
  GetFileSize p -> PR.getFileSize p
  DoesFileExist p -> PR.doesFileExist p
  DoesDirectoryExist p -> PR.doesDirectoryExist p

#if OSX
  GetXdgDirectory {} ->
    pure (FsUtils.unsafeEncodeFpToOs $ concatDirs ["test", "integration", "toml", "osx"])
#else
  GetXdgDirectory {} ->
    pure (FsUtils.unsafeEncodeFpToOs $ concatDirs ["test", "integration", "toml"])
#endif
  _ -> error "runPathReaderConfigIO: unimplemented"

{- ORMOLU_ENABLE -}

runTerminalConfigIO ::
  ( IOE :> es,
    IORefStatic :> es,
    Reader (IORef [Text]) :> es
  ) =>
  Eff (TerminalDynamic : es) a ->
  Eff es a
runTerminalConfigIO = reinterpret Term.runTerminalDynamicIO $ \_ -> \case
  PutStrLn t -> ask >>= (`modifyIORef'` (T.pack t :))
  GetTerminalSize -> Term.getTerminalSize
  _ -> error "runTerminalConfigIO: unimplemented"

runDBusConfigIO ::
  Eff (DBusDynamic : es) a ->
  Eff es a
runDBusConfigIO = interpret $ \_ -> \case
  DBus.ConnectSession ->
    pure
      $ Client
        { clientSocket = error "todo",
          clientPendingCalls = error "todo",
          clientSignalHandlers = error "todo",
          clientObjects = error "todo",
          clientThreadID = error "todo",
          clientInterfaces = error "todo"
        }
  DBus.Notify {} -> error "runDBusConfigIO.Notify: unimplemented"

runNoConfigIO ::
  Eff
    [ DBusDynamic,
      TerminalDynamic,
      PathWriterStatic,
      PathReaderDynamic,
      OptparseStatic,
      HandleWriterStatic,
      FileWriterStatic,
      FileReaderStatic,
      IORefStatic,
      Reader (IORef [Text]),
      Environment,
      Concurrent,
      IOE
    ]
    a ->
  IORef [Text] ->
  IO a
runNoConfigIO m ref = run m
  where
    run =
      runEff
        . runConcurrent
        . Env.runEnvironment
        . runReader ref
        . runIORefStaticIO
        . FR.runFileReaderStaticIO
        . FW.runFileWriterStaticIO
        . HW.runHandleWriterStaticIO
        . runOptparseStaticIO
        . runPathReaderNoConfigIO
        . PW.runPathWriterStaticIO
        . runTerminalConfigIO
        . runDBusConfigIO

runPathReaderNoConfigIO ::
  ( IOE :> es
  ) =>
  Eff (PathReaderDynamic : es) a ->
  Eff es a
runPathReaderNoConfigIO = reinterpret PR.runPathReaderStaticIO $ \_ -> \case
  DoesFileExist p -> PR.doesFileExist p
  GetXdgDirectory {} -> pure [osp|./|]
  _ -> error "runPathReaderNoConfigIO: unimplemented"

-- | Used to check our result Env against our expectations. Very similar to
-- real Env, except some types are simplified:
--
-- * FileLogging is merely a bool since we just want to check off/on, not
--   equality with a file handle or queue.
data SimpleEnv = MkSimpleEnv
  { timeout :: Maybe Timeout,
    init :: Maybe Text,
    keyHide :: KeyHide,
    pollInterval :: PollInterval,
    timerFormat :: TimerFormat,
    cmdNameTrunc :: Maybe (Truncation TCmdName),
    cmdLog :: Bool,
    cmdLogLineTrunc :: Maybe (Truncation TCmdLine),
    cmdLogStripControl :: Maybe StripControl,
    fileLog :: Bool,
    fileLogStripControl :: Maybe StripControl,
    notifySystem :: Maybe NotifySystemP1,
    notifyAction :: Maybe NotifyAction,
    notifyTimeout :: Maybe NotifyTimeout,
    commands :: NESeq CommandP1
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''SimpleEnv

simplifyEnv :: Getter Env SimpleEnv
simplifyEnv = to $ \env ->
  MkSimpleEnv
    { timeout = env ^. #timeout,
      init = env ^. #init,
      keyHide = env ^. (#logging % #keyHide),
      pollInterval = env ^. (#logging % #pollInterval),
      timerFormat = env ^. (#logging % #timerFormat),
      cmdLog = is (#logging % #cmdLog % _Just) env,
      cmdNameTrunc = env ^. (#logging % #cmdNameTrunc),
      cmdLogLineTrunc = env ^? (#logging % #cmdLog %? #lineTrunc % _Just),
      cmdLogStripControl = env ^? (#logging % #cmdLog %? #stripControl),
      fileLog = isJust (env ^. (#logging % #fileLog)),
      fileLogStripControl = env ^? (#logging % #fileLog %? #stripControl),
      notifySystem = mkNotifySystem env,
      notifyAction = env ^? (#notifyEnv %? #action),
      notifyTimeout = env ^? (#notifyEnv %? #timeout),
      commands = env ^. #commands
    }
  where
    -- Convert Phase2 back to Phase1 for Eq
    mkNotifySystem e = case e ^? (#notifyEnv %? #system) of
      Nothing -> Nothing
      Just NotifySend -> Just NotifySend
      Just (DBus _) -> Just (DBus ())
      Just AppleScript -> Just AppleScript

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  forall es.
  ( Concurrent :> es,
    DBusDynamic :> es,
    Environment :> es,
    FileReaderStatic :> es,
    FileWriterStatic :> es,
    HandleWriterStatic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    PathWriterStatic :> es,
    TerminalDynamic :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  -- | Expectation
  SimpleEnv ->
  Assertion
makeEnvAndVerify args toIO expected = do
  result <- toIO $ Env.withArgs args (withEnv pure)
  expected @=? result ^. simplifyEnv
