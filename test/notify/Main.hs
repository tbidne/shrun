{-# LANGUAGE CPP #-}
-- see NOTE: [Unused Top Binds]
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Runs functional tests.
module Main (main) where

import Data.Text qualified as T
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun (shrun)
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCommandLogging (getCommandLogging),
    HasCommands (getCleanup, getCommandDepGraph, getCommandStatusMap),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getHasTimedOut, getTimeout),
  )
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        displayRegions,
        logGlobal,
        logRegion,
        regionList,
        withRegion
      ),
  )
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Entry point for functional tests.
main :: IO ()
main = guardOrElse' "TEST_NOTIFY" ExpectEnvSet runTests dontRun
  where
    runTests = do
      setUncaughtExceptionHandler (putStrLn . displayException)
      defaultMain tests

    dontRun = putStrLn "*** Notify tests disabled. Enable with TEST_NOTIFY=1 ***"

tests :: TestTree
tests = do
  testGroup
    "Notify Tests"
    osTests

osTests :: List TestTree

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
      [ "--config",
        "off",
        "--common-log-key-hide",
        "on",
        "--notify-action-complete",
        "all",
        "--notify-system",
        "notify-send",
        "--notify-timeout",
        "5",
        "--config",
        "examples/config.toml",
        "frontend"
      ]
#endif

mkTest :: String -> TestTree
mkTest system = testCase ("Runs notify with " ++ system) $ do
  runShrunNoConfig (mkArgs system)

mkArgs :: String -> List String
mkArgs system =
  [ "--notify-action-complete",
    "all",
    "--notify-system",
    system,
    "--notify-timeout",
    "5",
    "sleep 2",
    "sleep 3"
  ]

-- NOTE: [Unused Top Binds]
--
-- Apparently, this warning is tripped as GHC accurately determines that
-- the consoleQueue field name is never used. It's kind of silly though since
-- we are using the other fields, and it's not like we can only create
-- one or two. Consider filing a GHC issue for this.

data NotificationsEnv = MkNotificationsEnv
  { unNotificationsEnv :: Env (),
    consoleQueue :: TBQueue (LogRegion ()),
    logsRef :: IORef (List Text)
  }

instance HasAnyError NotificationsEnv where
  getAnyError = getAnyError . (.unNotificationsEnv)

instance HasCommands NotificationsEnv where
  getCleanup = getCleanup . (.unNotificationsEnv)
  getCommandDepGraph = getCommandDepGraph . (.unNotificationsEnv)
  getCommandStatusMap = getCommandStatusMap . (.unNotificationsEnv)

instance HasCommandLogging NotificationsEnv where
  getCommandLogging = getCommandLogging . (.unNotificationsEnv)

instance HasCommonLogging NotificationsEnv where
  getCommonLogging = getCommonLogging . (.unNotificationsEnv)

instance HasConsoleLogging NotificationsEnv () where
  getConsoleLogging = getConsoleLogging . (.unNotificationsEnv)

instance HasFileLogging NotificationsEnv where
  getFileLogging = getFileLogging . (.unNotificationsEnv)

instance HasInit NotificationsEnv where
  getInit = getInit . (.unNotificationsEnv)

instance HasNotifyConfig NotificationsEnv where
  getNotifyConfig = getNotifyConfig . (.unNotificationsEnv)

instance HasTimeout NotificationsEnv where
  getTimeout = getTimeout . (.unNotificationsEnv)

  getHasTimedOut = getHasTimedOut . (.unNotificationsEnv)

liftNotify :: ShellT (Env ()) IO a -> ShellT NotificationsEnv IO a
liftNotify m = do
  MkNotificationsEnv env _ _ <- ask
  liftIO $ runShellT m env

instance MonadRegionLogger (ShellT NotificationsEnv IO) where
  type Region (ShellT NotificationsEnv IO) = ()

  logGlobal t = asks (.logsRef) >>= \ref -> modifyIORef' ref (t :)
  logRegion _ _ t = asks (.logsRef) >>= \ref -> modifyIORef' ref (t :)
  withRegion _ onRegion = onRegion ()
  displayRegions m = m
  regionList = atomically $ newTMVar []

runShrunNoConfig :: List String -> IO ()
runShrunNoConfig = runShrun . (["--config", "off"] ++)

runShrun :: List String -> IO ()
runShrun args = do
  consoleQueue <- newTBQueueA 1
  logsRef <- newIORef' []
  eSomeEx <-
    tryMySync
      $ withArgs
        args
        ( withEnv
            ( \env ->
                runShellT shrun
                  $ MkNotificationsEnv env consoleQueue logsRef
            )
        )

  case eSomeEx of
    Right () -> pure ()
    Left ex -> do
      logs <- readIORef' logsRef

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
