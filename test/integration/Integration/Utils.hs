{-# LANGUAGE TemplateHaskell #-}

module Integration.Utils
  ( ConfigIO (..),
    _MkConfigIO,
    NoConfigIO (..),
    _MkNoConfigIO,
    makeEnvAndVerify,
  )
where

import Data.Maybe (isJust)
import Integration.Prelude as X
import ShellRun.Configuration.Env (makeEnv)
import ShellRun.Configuration.Env.Types
  ( CmdDisplay,
    CmdLogging,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import ShellRun.Data.Command (Command)
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Effects.Atomic (Atomic (..))
import ShellRun.Effects.FileSystemReader (FileSystemReader (..))
import ShellRun.Effects.Terminal (Terminal (..))
import System.Environment (withArgs)

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (IO a)
  deriving
    ( Applicative,
      Atomic,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Terminal
    )
    via IO

makePrisms ''ConfigIO

instance FileSystemReader ConfigIO where
  getXdgConfig _ = pure "test/integration/toml"
  readFile = liftIO . readFile
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (IO a)
  deriving
    ( Applicative,
      Atomic,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Terminal
    )
    via IO

makePrisms ''NoConfigIO

instance FileSystemReader NoConfigIO where
  getXdgConfig _ = pure "./"
  readFile = liftIO . readFile
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  ( Atomic m,
    FileSystemReader m,
    MonadUnliftIO m,
    Terminal m
  ) =>
  -- | List of CLI arguments.
  [String] ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  Maybe Timeout ->
  Maybe FilePath ->
  CmdLogging ->
  CmdDisplay ->
  Maybe (Truncation 'TCmdName) ->
  Maybe (Truncation 'TCmdLine) ->
  StripControl ->
  Bool ->
  NonEmptySeq Command ->
  Assertion
makeEnvAndVerify
  args
  toIO
  timeout
  fileLogging
  cmdLogging
  cmdDisplay
  cmdNameTrunc
  cmdLineTrunc
  stripControl
  disableLogging
  commands = do
    result <- toIO $ withRunInIO (\runner -> withArgs args (runner makeEnv))

    timeout @=? result ^. #timeout
    fileLogging @=? result ^? (#fileLogging %? _1)
    cmdLogging @=? result ^. #cmdLogging
    cmdDisplay @=? result ^. #cmdDisplay
    cmdNameTrunc @=? result ^. #cmdNameTrunc
    stripControl @=? result ^. #stripControl
    disableLogging @=? result ^. #disableLogging
    commands @=? result ^. #commands

    -- Because the 'detect' option will read variable widths, depending on
    -- the terminal size. We use 'Just 0' as a sentinel for "do not test the
    -- actual value".
    let resultcmdLineTrunc = result ^. #cmdLineTrunc
    case cmdLineTrunc of
      Just 0 ->
        assertBool
          ("Should be just " <> show resultcmdLineTrunc)
          (isJust $ result ^. #cmdLineTrunc)
      other -> other @=? resultcmdLineTrunc
