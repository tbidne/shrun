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

import Integration.Prelude as X
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types (CmdDisplay, CmdLogging, Env, StripControl, TruncRegion (..), Truncation)
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Timeout (Timeout)
import Shrun.Effects.FileSystemReader (FileSystemReader (..))
import Shrun.Effects.FileSystemWriter (FileSystemWriter (..))
import Shrun.Effects.Mutable (Mutable (..))
import Shrun.Effects.Terminal (Terminal (..))
import System.Environment (withArgs)

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (ReaderT (IORef [Text]) IO a)
  deriving
    ( Applicative,
      FileSystemWriter,
      Functor,
      Monad,
      MonadIO,
      MonadReader (IORef [Text]),
      MonadUnliftIO,
      Mutable
    )
    via (ReaderT (IORef [Text])) IO

runConfigIO :: ConfigIO a -> IORef [Text] -> IO a
runConfigIO (MkConfigIO rdr) = runReaderT rdr

instance FileSystemReader ConfigIO where
  getXdgConfig _ = pure "test/integration/toml"
  readFile = liftIO . readFile
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

instance Terminal ConfigIO where
  putTextLn t = ask >>= (`modifyIORef'` (t :))

  -- hardcoded so we can test 'detect'
  getTerminalWidth = pure 0
  sleep = liftIO . sleep

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (ReaderT (IORef [Text]) IO a)
  deriving
    ( Applicative,
      FileSystemWriter,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Mutable
    )
    via (ReaderT (IORef [Text])) IO

runNoConfigIO :: NoConfigIO a -> IORef [Text] -> IO a
runNoConfigIO (MkNoConfigIO rdr) = runReaderT rdr

instance FileSystemReader NoConfigIO where
  getXdgConfig _ = pure "./"
  readFile = liftIO . readFile
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

deriving via ConfigIO instance Terminal NoConfigIO

-- | Used to check our result Env against our expectations. Very similar to
-- real Env, except some types are simplified:
--
-- * FileLogging is merely a bool since we just want to check off/on, not
--   equality with a file handle or queue.
data SimpleEnv = MkSimpleEnv
  { timeout :: Maybe Timeout,
    fileLogging :: Bool,
    fileLogStripControl :: StripControl,
    cmdLogging :: CmdLogging,
    cmdDisplay :: CmdDisplay,
    cmdNameTrunc :: Maybe (Truncation 'TCmdName),
    cmdLineTrunc :: Maybe (Truncation 'TCmdLine),
    stripControl :: StripControl,
    disableLogging :: Bool,
    commands :: NonEmptySeq Command
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''SimpleEnv

simplifyEnv :: Getter Env SimpleEnv
simplifyEnv = to $ \env ->
  MkSimpleEnv
    { timeout = env ^. #timeout,
      fileLogging = m2b (env ^. #fileLogging),
      fileLogStripControl = env ^. #fileLogStripControl,
      cmdLogging = env ^. #cmdLogging,
      cmdDisplay = env ^. #cmdDisplay,
      cmdNameTrunc = env ^. #cmdNameTrunc,
      cmdLineTrunc = env ^. #cmdLineTrunc,
      stripControl = env ^. #stripControl,
      disableLogging = env ^. #disableLogging,
      commands = env ^. #commands
    }

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  forall m.
  ( FileSystemReader m,
    FileSystemWriter m,
    MonadUnliftIO m,
    Mutable m,
    Terminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  -- | Expectation
  SimpleEnv ->
  Assertion
makeEnvAndVerify
  args
  toIO
  expected = do
    result <- toIO $ withRunInIO $ \runner ->
      withArgs args (runner (withEnv pure))

    expected @=? result ^. simplifyEnv
