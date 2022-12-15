{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functional.Prelude
  ( module X,

    -- * Running tests
    runAndGetLogs,

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
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( HasCommands (..),
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
    logs :: !(IORef (List Text))
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

-- | @since 0.3
instance MonadRegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

runAndGetLogs :: List String -> IO (IORef (List Text))
runAndGetLogs argList = do
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
              commands = env ^. #commands,
              logs = ls
            }
    SR.runShellT SR.shrun funcEnv
    pure $ funcEnv ^. #logs

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
withErrorPrefix :: (IsString s, Semigroup s) => s -> s -> s
withErrorPrefix cmd txt = "[Error][" <> cmd <> "] " <> txt

-- | Expected timing text.
withTimerPrefix :: (Semigroup a, IsString a) => a -> a
withTimerPrefix = (timerPrefix <>)

-- | Expected timing text.
withTimeoutPrefix :: (Semigroup a, IsString a) => a -> a
withTimeoutPrefix = (timeoutPrefix <>)

withFinishedPrefix :: (Semigroup s, IsString s) => s -> s
withFinishedPrefix = (finishedPrefix <>)
