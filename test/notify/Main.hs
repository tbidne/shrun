{-# LANGUAGE CPP #-}

-- | Runs functional tests.
module Main (main) where

import Data.Text qualified as T
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun (shrun)
import Shrun.Env (withEnv)
import Shrun.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCommands (getCommands, getCompletedCmds),
    HasInit (getInit),
    HasLogging (getLogging),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    Logging
      ( MkLogging,
        cmdLog,
        cmdLogReadSize,
        cmdNameTrunc,
        consoleLog,
        fileLog,
        keyHide,
        pollInterval,
        timerFormat
      ),
  )
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.Logging.Types (LogRegion)
import Shrun.Notify.MonadNotify (MonadNotify (notify))
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Entry point for functional tests.
main :: IO ()
main = guardOrElse' "NOTIFY_TESTS" ExpectEnvSet runTests dontRun
  where
    runTests = do
      setUncaughtExceptionHandler (putStrLn . displayException)
      defaultMain tests

    dontRun = putStrLn "*** Notify tests disabled. Enable with NOTIFY_TESTS=1 ***"

tests :: TestTree
tests = do
  testGroup
    "Notify Tests"
    osTests

osTests :: [TestTree]

#if OSX
osTests =
  [ mkTest "apple-script"
  ]
#else
osTests =
  [ mkTest "dbus",
    mkTest "notify-send",
    notifySendHandlesLegendQuotes
  ]

-- This test for a bug where notify-send could not cope with quotation marks
-- in legend file commands.
notifySendHandlesLegendQuotes :: TestTree
notifySendHandlesLegendQuotes = testCase "notify-send handles legend quotes" $ do
  runShrun args
  where
    args =
      [ "--log-key-hide",
        "--notify-action",
        "all",
        "--notify-system",
        "notify-send",
        "--config",
        "examples/config.toml",
        "ui"
      ]
#endif

mkTest :: String -> TestTree
mkTest system = testCase ("Runs notify with " ++ system) $ do
  runShrunNoConfig (mkArgs system)

mkArgs :: String -> List String
mkArgs system =
  [ "--notify-action",
    "all",
    "--notify-system",
    system,
    "--notify-timeout",
    "5",
    "sleep 2",
    "sleep 3"
  ]

data NotifyEnv = MkNotifyEnv
  { unNotifyEnv :: Env,
    consoleQueue :: TBQueue (LogRegion ()),
    logsRef :: IORef [Text]
  }

instance HasAnyError NotifyEnv where
  getAnyError = getAnyError . (.unNotifyEnv)

instance HasCommands NotifyEnv where
  getCommands = getCommands . (.unNotifyEnv)
  getCompletedCmds = getCompletedCmds . (.unNotifyEnv)

instance HasInit NotifyEnv where
  getInit = getInit . (.unNotifyEnv)

instance HasNotifyConfig NotifyEnv where
  getNotifyConfig = getNotifyConfig . (.unNotifyEnv)

instance HasTimeout NotifyEnv where
  getTimeout = getTimeout . (.unNotifyEnv)

instance HasLogging NotifyEnv () where
  getLogging nenv =
    MkLogging
      { keyHide = envLogging ^. #keyHide,
        pollInterval = envLogging ^. #pollInterval,
        cmdLogReadSize = envLogging ^. #cmdLogReadSize,
        timerFormat = envLogging ^. #timerFormat,
        cmdNameTrunc = envLogging ^. #cmdNameTrunc,
        cmdLog = envLogging ^. #cmdLog,
        fileLog = envLogging ^. #fileLog,
        consoleLog = nenv.consoleQueue
      }
    where
      envLogging :: Logging ConsoleRegion
      envLogging = getLogging nenv.unNotifyEnv

liftNotify :: ShellT Env IO a -> ShellT NotifyEnv IO a
liftNotify m = do
  MkNotifyEnv env _ _ <- ask
  liftIO $ runShellT m env

instance MonadNotify (ShellT NotifyEnv IO) where
  notify = liftNotify . notify

instance MonadRegionLogger (ShellT NotifyEnv IO) where
  type Region (ShellT NotifyEnv IO) = ()

  logGlobal t = asks (.logsRef) >>= \ref -> modifyIORef' ref (t :)
  logRegion _ _ t = asks (.logsRef) >>= \ref -> modifyIORef' ref (t :)
  withRegion _ onRegion = onRegion ()
  displayRegions m = m

runShrunNoConfig :: List String -> IO ()
runShrunNoConfig = runShrun . ("--no-config" :)

runShrun :: List String -> IO ()
runShrun args = do
  consoleQueue <- newTBQueueA 1
  logsRef <- newIORef []
  eSomeEx <-
    tryAny
      $ withArgs
        args
        ( withEnv
            ( \env ->
                runShellT shrun
                  $ MkNotifyEnv env consoleQueue logsRef
            )
        )

  case eSomeEx of
    Right () -> pure ()
    Left ex -> do
      logs <- readIORef logsRef

      let formatted = T.intercalate "\n" logs
          err =
            mconcat
              [ "Encountered exception\n\n",
                "Logs:\n\n",
                T.unpack formatted,
                "\n\nException message: ",
                displayException ex
              ]

      assertFailure err
