{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Utils
  ( ConfigIO (..),
    runConfigIO,
    NoConfigIO (..),
    runNoConfigIO,
    SimpleEnv (..),
    makeEnvAndVerify,
  )
where

import Data.Text qualified as T
import Effects.MonadFs (MonadFsReader (..))
import Effects.MonadTerminal (MonadTerminal (..))
import Integration.Prelude as X
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    Env,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Timeout (Timeout)

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (ReaderT (IORef [Text]) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCallStack,
      MonadFsWriter,
      MonadIO,
      MonadIORef,
      MonadReader (IORef [Text]),
      MonadTBQueue,
      MonadTVar,
      MonadUnliftIO
    )
    via (ReaderT (IORef [Text])) IO

runConfigIO :: ConfigIO a -> IORef [Text] -> IO a
runConfigIO (MkConfigIO rdr) = runReaderT rdr

instance MonadFsReader ConfigIO where
  getFileSize = liftIO . getFileSize
  getHomeDirectory = liftIO getHomeDirectory
  getXdgConfig _ = pure "test/integration/toml"
  readFile = liftIO . readFile
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist
  doesPathExist = liftIO . doesPathExist
  canonicalizePath = liftIO . canonicalizePath
  listDirectory = liftIO . listDirectory

instance MonadTerminal ConfigIO where
  putStr = error "putStr: unimplemented"

  -- capture logs
  putStrLn t = ask >>= (`modifyIORef'` (T.pack t :))

  getChar = error "getChar: unimplemented"

  -- hardcoded so we can test 'detect'
  getTerminalSize = liftIO getTerminalSize

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (ReaderT (IORef [Text]) IO a)
  deriving
    ( Applicative,
      MonadFsWriter,
      Functor,
      Monad,
      MonadCallStack,
      MonadIO,
      MonadTBQueue,
      MonadTVar,
      MonadUnliftIO
    )
    via (ReaderT (IORef [Text])) IO

runNoConfigIO :: NoConfigIO a -> IORef [Text] -> IO a
runNoConfigIO (MkNoConfigIO rdr) = runReaderT rdr

instance MonadFsReader NoConfigIO where
  getXdgConfig _ = pure "./"
  getHomeDirectory = error "getHomeDirectory: unimplemented"
  readFile = liftIO . readFile
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist
  doesPathExist = liftIO . doesPathExist
  canonicalizePath = liftIO . canonicalizePath
  listDirectory = liftIO . listDirectory

deriving via ConfigIO instance MonadTerminal NoConfigIO

-- | Used to check our result Env against our expectations. Very similar to
-- real Env, except some types are simplified:
--
-- * FileLogging is merely a bool since we just want to check off/on, not
--   equality with a file handle or queue.
data SimpleEnv = MkSimpleEnv
  { timeout :: !(Maybe Timeout),
    cmdDisplay :: !CmdDisplay,
    cmdLogging :: !Bool,
    cmdLogNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    cmdLogLineTrunc :: !(Maybe (Truncation 'TCmdLine)),
    cmdLogStripControl :: !(Maybe StripControl),
    fileLogging :: !Bool,
    fileLogStripControl :: !(Maybe StripControl),
    commands :: !(NonEmptySeq Command)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''SimpleEnv

simplifyEnv :: Getter Env SimpleEnv
simplifyEnv = to $ \env ->
  MkSimpleEnv
    { timeout = env ^. #timeout,
      cmdDisplay = env ^. (#logging % #cmdDisplay),
      cmdLogging = is (#logging % #cmdLogging % _Just) env,
      cmdLogNameTrunc = env ^. (#logging % #cmdNameTrunc),
      cmdLogLineTrunc = env ^? (#logging % #cmdLogging %? #lineTrunc % _Just),
      cmdLogStripControl = env ^? (#logging % #cmdLogging %? #stripControl),
      fileLogging = m2b (env ^. (#logging % #fileLogging)),
      fileLogStripControl = env ^? (#logging % #fileLogging %? #stripControl),
      commands = env ^. #commands
    }

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  forall m.
  ( MonadCallStack m,
    MonadFsReader m,
    MonadFsWriter m,
    MonadTBQueue m,
    MonadTerminal m,
    MonadTVar m,
    MonadUnliftIO m
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
