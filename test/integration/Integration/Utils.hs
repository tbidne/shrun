{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Utils
  ( ConfigIO (..),
    runConfigIO,
    NoConfigIO (..),
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
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( getHomeDirectory,
        getXdgDirectory
      ),
  )
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.System.Terminal (MonadTerminal (getChar, getTerminalSize))
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
import Shrun.Notify.MonadDBus (MonadDBus (connectSession, notify))
import Shrun.Notify.MonadNotifySend (MonadNotifySend (notify))
import Shrun.Notify.Types
  ( NotifyAction,
    NotifySystem (AppleScript, DBus, NotifySend),
    NotifySystemP1,
    NotifyTimeout,
  )

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (ReaderT (IORef [Text]) IO a)
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
      MonadReader (IORef [Text]),
      MonadSTM,
      MonadThrow
    )
    via (ReaderT (IORef [Text])) IO

runConfigIO :: ConfigIO a -> IORef [Text] -> IO a
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
    pure (FsUtils.unsafeEncodeFpToOs $ concatDirs ["test", "integration", "toml", "osx"])
#else
  getXdgDirectory _ _ =
    pure (FsUtils.unsafeEncodeFpToOs $ concatDirs ["test", "integration", "toml"])
#endif

instance MonadTerminal ConfigIO where
  putStr = error "putStr: unimplemented"

  -- capture logs
  putStrLn t = ask >>= (`modifyIORef'` (T.pack t :))

  getChar = error "getChar: unimplemented"

  -- hardcoded so we can test 'detect'
  getTerminalSize = liftIO getTerminalSize

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

instance MonadNotifySend ConfigIO where
  notify = error "notify: unimplemented"

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (ReaderT (IORef [Text]) IO a)
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
    via (ReaderT (IORef [Text])) IO
  deriving
    (MonadDBus, MonadNotifySend)
    via ConfigIO

runNoConfigIO :: NoConfigIO a -> IORef [Text] -> IO a
runNoConfigIO (MkNoConfigIO rdr) = runReaderT rdr

instance MonadPathReader NoConfigIO where
  getXdgDirectory _ _ = pure [osp|./|]
  getHomeDirectory = error "getHomeDirectory: unimplemented"
  doesFileExist = liftIO . doesFileExist

deriving via ConfigIO instance MonadTerminal NoConfigIO

-- | Used to check our result Env against our expectations. Very similar to
-- real Env, except some types are simplified:
--
-- * FileLogging is merely a bool since we just want to check off/on, not
--   equality with a file handle or queue.
data SimpleEnv = MkSimpleEnv
  { timeout :: !(Maybe Timeout),
    init :: !(Maybe Text),
    keyHide :: !KeyHide,
    pollInterval :: !PollInterval,
    timerFormat :: !TimerFormat,
    cmdNameTrunc :: !(Maybe (Truncation TCmdName)),
    cmdLog :: !Bool,
    cmdLogLineTrunc :: !(Maybe (Truncation TCmdLine)),
    cmdLogStripControl :: !(Maybe StripControl),
    fileLog :: !Bool,
    fileLogStripControl :: !(Maybe StripControl),
    notifySystem :: !(Maybe NotifySystemP1),
    notifyAction :: !(Maybe NotifyAction),
    notifyTimeout :: !(Maybe NotifyTimeout),
    commands :: !(NESeq CommandP1)
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
  forall m.
  ( MonadDBus m,
    MonadEnv m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadMask m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  -- | Expectation
  SimpleEnv ->
  Assertion
makeEnvAndVerify args toIO expected = do
  result <- toIO $ withArgs args (withEnv pure)
  expected @=? result ^. simplifyEnv
