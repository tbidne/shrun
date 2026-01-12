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
import Shrun.Notify.MonadNotify (MonadNotify (notify))
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
        "ui"
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

data NotifyEnv = MkNotifyEnv
  { unNotifyEnv :: Env (),
    consoleQueue :: TBQueue (LogRegion ()),
    logsRef :: IORef (List Text)
  }

instance HasAnyError NotifyEnv where
  getAnyError = getAnyError . (.unNotifyEnv)

instance HasCommands NotifyEnv where
  getCleanup = getCleanup . (.unNotifyEnv)
  getCommandDepGraph = getCommandDepGraph . (.unNotifyEnv)
  getCommandStatusMap = getCommandStatusMap . (.unNotifyEnv)

instance HasCommandLogging NotifyEnv where
  getCommandLogging = getCommandLogging . (.unNotifyEnv)

instance HasCommonLogging NotifyEnv where
  getCommonLogging = getCommonLogging . (.unNotifyEnv)

instance HasConsoleLogging NotifyEnv () where
  getConsoleLogging = getConsoleLogging . (.unNotifyEnv)

instance HasFileLogging NotifyEnv where
  getFileLogging = getFileLogging . (.unNotifyEnv)

instance HasInit NotifyEnv where
  getInit = getInit . (.unNotifyEnv)

instance HasNotifyConfig NotifyEnv where
  getNotifyConfig = getNotifyConfig . (.unNotifyEnv)

instance HasTimeout NotifyEnv where
  getTimeout = getTimeout . (.unNotifyEnv)

  getHasTimedOut = getHasTimedOut . (.unNotifyEnv)

liftNotify :: ShellT (Env ()) IO a -> ShellT NotifyEnv IO a
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
  regionList = atomically $ newTMVar []

runShrunNoConfig :: List String -> IO ()
runShrunNoConfig = runShrun . (["--config", "off"] ++)

runShrun :: List String -> IO ()
runShrun args = do
  consoleQueue <- newTBQueueA 1
  logsRef <- newIORef []
  eSomeEx <-
    tryMySync
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
