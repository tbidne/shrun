{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.Prelude
  ( module X,

    -- * Running tests
    run,
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
  )
where

import Data.Sequence (Seq)
import Data.String as X (IsString)
import Data.Typeable (typeRep)
import Effects.MonadCallStack (AnnotatedException)
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( HasAnyError (..),
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),
    Logging (..),
  )
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Timeout (Timeout)
import Shrun.Logging (MonadRegionLogger (..))
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
    logging :: !(Logging ()),
    completedCmds :: !(TVar (Seq Command)),
    commands :: !(NonEmptySeq Command),
    logs :: !(IORef (List Text)),
    anyError :: !(TVar Bool)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view #timeout

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

-- | @since 0.3
instance MonadRegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

-- | Runs the args and retrieves the logs.
run :: List String -> IO (IORef (List Text))
run = runMaybeException ExNothing

-- | 'runException' specialized to ExitFailure.
runExitFailure :: List String -> IO (IORef (List Text))
runExitFailure =
  runMaybeException
    (ExJust $ Proxy @(AnnotatedException ExitCode))

-- | Like 'runException', except it expects an exception.
runException ::
  forall e.
  Exception e =>
  List String ->
  IO (IORef (List Text))
runException = runMaybeException (ExJust (Proxy @e))

-- | So we can hide the exception type and make it so run does not
-- have to pass in a dummy var to runMaybeException.
data MaybeException where
  ExNothing :: MaybeException
  ExJust :: Exception e => Proxy e -> MaybeException

runMaybeException ::
  MaybeException ->
  List String ->
  IO (IORef (List Text))
runMaybeException mException argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    ls <- newIORef []
    consoleQueue <- newTBQueueM 1_000
    let funcEnv =
          MkFuncEnv
            { timeout = env ^. #timeout,
              -- doing this by hand since we need a different consoleLogging
              logging =
                MkLogging
                  { cmdDisplay = env ^. (#logging % #cmdDisplay),
                    cmdNameTrunc = env ^. (#logging % #cmdNameTrunc),
                    cmdLogging = env ^. (#logging % #cmdLogging),
                    consoleLogging = consoleQueue,
                    fileLogging = env ^. (#logging % #fileLogging)
                  },
              completedCmds = env ^. #completedCmds,
              anyError = env ^. #anyError,
              commands = env ^. #commands,
              logs = ls
            }

    case mException of
      ExNothing -> SR.runShellT SR.shrun funcEnv $> funcEnv ^. #logs
      ExJust (proxy :: Proxy e) ->
        try @e (SR.runShellT SR.shrun funcEnv) >>= \case
          Left _ -> pure $ funcEnv ^. #logs
          Right _ ->
            error $
              mconcat
                [ "Expected exception <",
                  show (typeRep proxy),
                  ">, received none"
                ]

commandPrefix :: IsString s => s
commandPrefix = "[Command]"

-- | Expected timer text.
timerPrefix :: IsString s => s
timerPrefix = "[Timer] "

-- | Expected timeout text.
timeoutPrefix :: IsString s => s
timeoutPrefix = "[Warn] Timed out, cancelling remaining commands: "

-- | Expected finished prefix.
finishedPrefix :: IsString s => s
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
