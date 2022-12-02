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
import Effects.MonadFsReader (MonadFsReader (..))
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
import Shrun.Effects.Mutable (Mutable (..))

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (ReaderT (IORef [Text]) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCallStack,
      MonadFsWriter,
      MonadIO,
      MonadReader (IORef [Text]),
      MonadThread,
      MonadUnliftIO,
      Mutable
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
      MonadUnliftIO,
      Mutable
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
    disableLogging :: !Bool,
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
      disableLogging = env ^. #disableLogging,
      cmdDisplay = env ^. #cmdDisplay,
      cmdLogging = is (#cmdLogging % _Just) env,
      cmdLogNameTrunc = env ^. #cmdNameTrunc,
      cmdLogLineTrunc = env ^? (#cmdLogging %? #lineTrunc % _Just),
      cmdLogStripControl = env ^? (#cmdLogging %? #stripControl),
      fileLogging = m2b (env ^. #fileLogging),
      fileLogStripControl = env ^? (#fileLogging %? #stripControl),
      commands = env ^. #commands
    }

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  forall m.
  ( MonadCallStack m,
    MonadFsReader m,
    MonadFsWriter m,
    MonadTerminal m,
    MonadUnliftIO m,
    Mutable m
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
