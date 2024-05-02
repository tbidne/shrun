{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.Prelude
  ( module X,

    -- * Running tests
    run,
    runNotes,
    runException,
    runExitFailure,
    runOuterExitFailure,
    runOuterException,

    -- * Expectations

    -- ** Text
    commandPrefix,
    timerPrefix,
    timeoutPrefix,
    finishedPrefix,

    -- ** Prefixes
    withCommandPrefix,
    withSuccessPrefix,
    withErrorPrefix,
    withTimerPrefix,
    withTimeoutPrefix,
    withFinishedPrefix,

    -- * Misc
    withBaseArgs,
    withNoConfig,
    notifySystemArg,
  )
where

import Data.String as X (IsString)
import Data.Typeable (typeRep)
import Effects.FileSystem.Utils (combineFilePaths)
import Effects.FileSystem.Utils as X (unsafeDecodeOsToFp, (</>!))
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCmdLogging (getCmdLogging),
    HasCommands (getCommands, getCompletedCmds),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
  )
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        displayRegions,
        logGlobal,
        logRegion,
        withRegion
      ),
  )
import Shrun.Notify.MonadNotify (MonadNotify (notify), ShrunNote)
import Shrun.Prelude as X
import Shrun.ShellT (ShellT)
import Test.Tasty as X (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

data FuncEnv = MkFuncEnv
  { coreEnv :: Env (),
    logs :: IORef (List Text),
    shrunNotes :: IORef (List ShrunNote)
  }

makeFieldLabelsNoPrefix ''FuncEnv

instance HasTimeout FuncEnv where
  getTimeout = getTimeout . view #coreEnv

instance HasInit FuncEnv where
  getInit = getInit . view #coreEnv

instance HasCommands FuncEnv where
  getCommands = getCommands . view #coreEnv
  getCompletedCmds = getCompletedCmds . view #coreEnv

instance HasAnyError FuncEnv where
  getAnyError = getAnyError . view #coreEnv

instance HasCmdLogging FuncEnv where
  getCmdLogging = getCmdLogging . view #coreEnv

instance HasCommonLogging FuncEnv where
  getCommonLogging = getCommonLogging . view #coreEnv

instance HasConsoleLogging FuncEnv () where
  getConsoleLogging = getConsoleLogging . view #coreEnv

instance HasFileLogging FuncEnv where
  getFileLogging = getFileLogging . view #coreEnv

instance HasNotifyConfig FuncEnv where
  getNotifyConfig = getNotifyConfig . view #coreEnv

instance MonadRegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

instance MonadNotify (ShellT FuncEnv IO) where
  notify note = do
    notesRef <- asks (view #shrunNotes)
    modifyIORef' notesRef (note :)
    pure Nothing

-- | Runs the args and retrieves the logs.
run :: List String -> IO (IORef (List Text))
run = fmap fst . runMaybeException ExNothing

-- | Runs the args and retrieves the sent notifications.
runNotes :: List String -> IO (IORef (List ShrunNote))
runNotes = fmap snd . runMaybeException ExNothing

-- | 'runException' specialized to ExitFailure.
runExitFailure :: List String -> IO (IORef (List Text))
runExitFailure =
  fmap fst . runMaybeException (ExJust $ Proxy @(ExceptionCS ExitCode))

-- | 'runOuterException' specialized to ExitFailure.
runOuterExitFailure :: List String -> IO (IORef (List Text))
runOuterExitFailure =
  fmap fst . runOuterException (Proxy @(ExceptionCS ExitCode))

-- | Like 'runException', except it expects an exception.
runException ::
  forall e.
  (Exception e) =>
  List String ->
  IO (IORef (List Text))
runException = fmap fst . runMaybeException (ExJust (Proxy @e))

-- | So we can hide the exception type and make it so run does not
-- have to pass in a dummy var to runMaybeException.
data MaybeException where
  ExNothing :: MaybeException
  ExJust :: (Exception e) => Proxy e -> MaybeException

-- | Runs shrun potentially catching an expected exception. Note that we only
-- catch exceptions from @runShellT shrun@. In particular, this means withEnv
-- config setup will __not__ perform any exception handling, since the
-- exception is caught before that.
runMaybeException ::
  MaybeException ->
  List String ->
  IO (IORef (List Text), IORef (List ShrunNote))
runMaybeException mException argList = do
  withArgs argList $ Env.withEnv $ \env -> do
    ls <- newIORef []
    shrunNotes <- newIORef []
    let funcEnv =
          MkFuncEnv
            { coreEnv = env,
              logs = ls,
              shrunNotes
            }

    case mException of
      ExNothing -> do
        SR.runShellT SR.shrun funcEnv $> (funcEnv ^. #logs, funcEnv ^. #shrunNotes)
      ExJust (proxy :: Proxy e) ->
        try @_ @e (SR.runShellT SR.shrun funcEnv) >>= \case
          Left _ -> pure (funcEnv ^. #logs, funcEnv ^. #shrunNotes)
          Right _ ->
            error
              $ mconcat
                [ "Expected exception <",
                  show (typeRep proxy),
                  ">, received none"
                ]

-- | Like 'runMaybeException' except:
--
-- 1. Always expects an exception.
-- 2. The __entire__ withEnv -> shrun process is surrounded with a catch.
--    This means an exception thrown by the main shrun process will trigger
--    withEnv's exception handling logic.
--
-- We need this when we want to test any non-happy-path logic in
-- withEnv, withCoreEnv, withMLogging, etc. i.e. anything at a higher scope
-- than 'SR.shrun'.
runOuterException ::
  forall e.
  (Exception e) =>
  Proxy e ->
  List String ->
  IO (IORef (List Text), IORef (List ShrunNote))
runOuterException proxy argList = do
  ls <- newIORef []
  shrunNotes <- newIORef []

  let action = do
        withArgs argList $ Env.withEnv $ \env -> do
          let funcEnv =
                MkFuncEnv
                  { coreEnv = env,
                    logs = ls,
                    shrunNotes
                  }

          SR.runShellT SR.shrun funcEnv

  try @_ @e action >>= \case
    Left _ -> pure (ls, shrunNotes)
    Right _ ->
      error
        $ mconcat
          [ "Expected exception <",
            show (typeRep proxy),
            ">, received none"
          ]

commandPrefix :: (IsString s) => s
commandPrefix = "[Command]"

-- | Expected timer text.
timerPrefix :: (IsString s) => s
timerPrefix = "[Timer] "

-- | Expected timeout text.
timeoutPrefix :: (IsString s) => s
timeoutPrefix = "[Warn] Timed out, cancelling remaining commands: "

-- | Expected finished prefix.
finishedPrefix :: (IsString s) => s
finishedPrefix = "[Finished] "

-- | Expected command text.
withCommandPrefix :: (IsString s, Semigroup s) => s -> s -> s
withCommandPrefix cmd txt = commandPrefix <> "[" <> cmd <> "] " <> txt

-- | Expected success text.
withSuccessPrefix :: (IsString s, Semigroup s) => s -> s
withSuccessPrefix txt = "[Success][" <> txt <> "] "

-- | Expected error text.
withErrorPrefix :: (IsString s, Semigroup s) => s -> s
withErrorPrefix cmd = "[Error][" <> cmd <> "] "

-- | Expected timing text.
withTimerPrefix :: (Semigroup a, IsString a) => a -> a
withTimerPrefix = (timerPrefix <>)

-- | Expected timing text.
withTimeoutPrefix :: (Semigroup a, IsString a) => a -> a
withTimeoutPrefix = (timeoutPrefix <>)

withFinishedPrefix :: (Semigroup s, IsString s) => s -> s
withFinishedPrefix = (finishedPrefix <>)

withBaseArgs :: [String] -> [String]
withBaseArgs as =
  [ "-c",
    configPath
  ]
    <> as

withNoConfig :: [String] -> [String]
withNoConfig as =
  [ "--no-config"
  ]
    <> as

configPath :: String
#if OSX
configPath = "examples" `cfp` "config_osx.toml"
#else
configPath = "examples" `cfp` "config.toml"
#endif

notifySystemArg :: String
#if OSX
notifySystemArg = "apple-script"
#else
notifySystemArg = "notify-send"
#endif

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths
