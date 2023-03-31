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
  )
where

import Data.String as X (IsString)
import Data.Typeable (typeRep)
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types (HasAnyError (..), HasCommands (..), HasInit (..), HasLogging (..), HasNotifyConfig (..), HasTimeout (..), Logging (..), NotifyEnv)
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Timeout (Timeout)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Notify.MonadNotify
import Shrun.Notify.Types
import Shrun.Prelude as X
import Shrun.ShellT (ShellT)
import System.Environment qualified as SysEnv
import System.Exit (ExitCode)
import Test.Tasty as X (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit as X (Assertion, testCase, (@=?))

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

-- | @since 0.3
data FuncEnv = MkFuncEnv
  { timeout :: !(Maybe Timeout),
    init :: !(Maybe Text),
    logging :: !(Logging ()),
    completedCmds :: !(TVar (Seq CommandP1)),
    commands :: !(NESeq CommandP1),
    logs :: !(IORef (List Text)),
    notifyEnv :: !(Maybe NotifyEnv),
    shrunNotes :: !(IORef (List ShrunNote)),
    anyError :: !(TVar Bool)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view #timeout

-- | @since 0.8
instance HasInit FuncEnv where
  getInit = view #init

-- | @since 0.3
instance HasLogging FuncEnv () where
  getLogging = view #logging

-- | @since 0.3
instance HasCommands FuncEnv where
  getCommands = view #commands
  getCompletedCmds = view #completedCmds

-- | @since 0.8
instance HasAnyError FuncEnv where
  getAnyError = view #anyError

instance HasNotifyConfig FuncEnv where
  getNotifyConfig env =
    (env ^. #notifyEnv) <&> \notifyEnv ->
      MkNotifyConfig
        { action = notifyEnv ^. #action,
          timeout = notifyEnv ^. #timeout
        }

-- | @since 0.3
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

runMaybeException ::
  MaybeException ->
  List String ->
  IO (IORef (List Text), IORef (List ShrunNote))
runMaybeException mException argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    ls <- newIORef []
    consoleQueue <- newTBQueueA 1_000
    shrunNotes <- newIORef []
    let funcEnv =
          MkFuncEnv
            { timeout = env ^. #timeout,
              init = env ^. #init,
              -- doing this by hand since we need a different consoleLogging
              logging =
                MkLogging
                  { keyHide = env ^. (#logging % #keyHide),
                    pollInterval = env ^. (#logging % #pollInterval),
                    cmdNameTrunc = env ^. (#logging % #cmdNameTrunc),
                    cmdLog = env ^. (#logging % #cmdLog),
                    consoleLog = consoleQueue,
                    fileLog = env ^. (#logging % #fileLog)
                  },
              completedCmds = env ^. #completedCmds,
              anyError = env ^. #anyError,
              commands = env ^. #commands,
              logs = ls,
              notifyEnv = env ^. #notifyEnv,
              shrunNotes
            }

    case mException of
      ExNothing -> do
        SR.runShellT SR.shrun funcEnv $> (funcEnv ^. #logs, funcEnv ^. #shrunNotes)
      ExJust (proxy :: Proxy e) ->
        try @_ @e (SR.runShellT SR.shrun funcEnv) >>= \case
          Left _ -> pure (funcEnv ^. #logs, funcEnv ^. #shrunNotes)
          Right _ ->
            error $
              mconcat
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
configPath = "examples" </> "config_osx.toml"
#else
configPath = "examples" </> "config.toml"
#endif
