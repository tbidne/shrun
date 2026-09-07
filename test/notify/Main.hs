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
import Shrun.Logging.RegionLogger
  ( RegionLogger
      ( DisplayRegions,
        LogGlobal,
        LogRegion,
        RegionList,
        WithRegion
      ),
  )
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude
import System.Environment qualified as Env
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import System.IO qualified as IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Entry point for functional tests.
main :: IO ()
main = guardOrElse' "TEST_NOTIFY" ExpectEnvSet runTests dontRun
  where
    runTests = do
      setUncaughtExceptionHandler (IO.putStrLn . displayException)
      defaultMain tests

    dontRun = IO.putStrLn "*** Notify tests disabled. Enable with TEST_NOTIFY=1 ***"

tests :: TestTree
tests = do
  testGroup
    "Notify Tests"
    (notifyIdenticalCommands : osTests)

-- Tests that our 'too many notifications' (dbus) error to warning logic
-- works. Might as well test osx too. Prior to the warning mitigation in
-- Shrun.Notify, this test indeed fails on my machine. With the fix, it
-- passes.
notifyIdenticalCommands :: TestTree
notifyIdenticalCommands = testCase desc $ do
  runShrun args
  where
    desc = "Tests identical notifications"
    args =
      [ "--config",
        "off",
        "--notify-action-start",
        "on",
        "--notify-action-complete",
        "all",
        "sleep 1",
        "sleep 1"
      ]

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
  { unNotificationsEnv :: Env NotifyEnv (),
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

instance HasNotifyConfig NotificationsEnv NotifyEnv where
  getNotifyConfig = getNotifyConfig . (.unNotificationsEnv)

instance HasTimeout NotificationsEnv where
  getTimeout = getTimeout . (.unNotificationsEnv)

  getHasTimedOut = getHasTimedOut . (.unNotificationsEnv)

runRegionLogger ::
  ( Concurrent :> es,
    Reader NotificationsEnv :> es,
    IOE :> es,
    Prim :> es
  ) =>
  Eff (RegionLogger () : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal t -> asks @NotificationsEnv (.logsRef) >>= \ref -> modifyIORef ref (t :)
  LogRegion _ _ t -> asks @NotificationsEnv (.logsRef) >>= \ref -> modifyIORef ref (t :)
  WithRegion _ onRegion -> localSeqUnliftIO env $ \unlift -> unlift $ onRegion ()
  DisplayRegions m -> localSeqUnliftIO env $ \unlift -> unlift m
  RegionList -> atomically $ newTMVar []

runShrunNoConfig :: List String -> IO ()
runShrunNoConfig = runShrun . (["--config", "off"] ++)

runShrun :: List String -> IO ()
runShrun args = do
  logsRef <- runEff $ runPrim $ newIORef []

  eSomeEx <- tryMySync $ Env.withArgs args $ runner $ withEnv $ \env -> do
    consoleQueue <- newTBQueueA 1
    let notifyEnv = MkNotificationsEnv env consoleQueue logsRef
    runReader notifyEnv
      . runRegionLogger
      $ shrun @NotificationsEnv @NotifyEnv @()

  case eSomeEx of
    Right () -> pure ()
    Left ex -> do
      logs <- runEff $ runPrim $ readIORef logsRef

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
  where
    runner =
      runEff
        . runConcurrent
        . runEnvironment
        . runProcess
        . runPrim
        . runNotify
        . runOptparse
        . runTime
        . runFileReader
        . runFileWriter
        . runHandleReader
        . runHandleWriter
        . runPathReader
        . runPathWriter
        . runPosixFiles
        . runPosixSignals
        . runTerminal
