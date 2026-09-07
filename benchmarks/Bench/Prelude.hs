{-# LANGUAGE UndecidableInstances #-}

module Bench.Prelude
  ( BenchEnv (..),
    runBench,
  )
where

import Effectful.Notify.Dynamic (Notify (InitNotifyEnv, Notify))
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError,
    HasCommandLogging,
    HasCommands,
    HasCommonLogging,
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging,
    HasInit,
    HasNotifyConfig (getNotifyConfig),
    HasTimeout,
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
import Shrun.Prelude
import System.Environment qualified as SysEnv

newtype BenchEnv = MkBenchEnv
  {unCoreEnv :: Env () ()}
  deriving
    ( HasAnyError,
      HasCommandLogging,
      HasCommands,
      HasCommonLogging,
      HasFileLogging,
      HasInit,
      HasTimeout
    )
    via (Env () ())

instance HasConsoleLogging BenchEnv () where
  getConsoleLogging = getConsoleLogging . (.unCoreEnv)

instance HasNotifyConfig BenchEnv () where
  getNotifyConfig = getNotifyConfig . (.unCoreEnv)

runRegionLoggerBench ::
  (Concurrent :> es) =>
  Eff (RegionLogger () : es) a ->
  Eff es a
runRegionLoggerBench = interpret $ \env -> \case
  LogGlobal _ -> pure ()
  LogRegion {} -> pure ()
  WithRegion _layout regionToShell -> localSeqUnlift env $ \unlift ->
    unlift (regionToShell ())
  DisplayRegions m -> localSeqUnlift env $ \unlift -> unlift m
  RegionList -> atomically $ newTMVar []

runNotifyBench ::
  Eff (Notify () : es) a ->
  Eff es a
runNotifyBench = interpret_ $ \case
  InitNotifyEnv _ -> pure ()
  Notify {} -> pure ()

runBench :: List String -> IO ()
runBench argList = do
  SysEnv.withArgs argList $ topHandler $ Env.withEnv $ \env -> do
    let benchEnv = MkBenchEnv env
    runReader benchEnv
      . runRegionLoggerBench
      . runHandleReader
      . runPosixSignals
      . runProcess
      . runTime
      $ SR.shrun @BenchEnv @() @()
  where
    topHandler =
      runEff
        . runConcurrent
        . runFileReader
        . runFileWriter
        . runHandleWriter
        . runPrim
        . runNotifyBench
        . runOptparse
        . runPathReader
        . runPathWriter
        . runPosixFiles
        . runTerminal
