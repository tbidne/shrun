{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Bench.Prelude
  ( BenchEnv (..),
    runBench,
  )
where

import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Environment qualified as Environment
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Optparse.Static qualified as OA
import Effectful.Process.Typed qualified as P
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Dynamic qualified as Time
import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( HasCommands (getCommands),
    HasInit (getInit),
    HasLogging (getLogging),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    Logging
      ( MkLogging,
        cmdLog,
        cmdNameTrunc,
        consoleLog,
        fileLog,
        keyHide,
        pollInterval,
        timerFormat
      ),
    NotifyEnv,
    ShrunState,
  )
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Timeout (Timeout)
import Shrun.Logging.RegionLogger
  ( RegionLoggerDynamic (DisplayRegions, LogGlobal, LogRegion, WithRegion),
  )
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.Notify (NotifyDynamic (Notify))
import Shrun.Notify.Types (NotifyConfig (MkNotifyConfig, action, timeout))
import Shrun.Prelude

data BenchEnv = MkBenchEnv
  { timeout :: Maybe Timeout,
    init :: Maybe Text,
    logging :: Logging (),
    commands :: NESeq CommandP1,
    notifyEnv :: Maybe NotifyEnv
  }

makeFieldLabelsNoPrefix ''BenchEnv

instance HasTimeout BenchEnv where
  getTimeout = view #timeout

instance HasInit BenchEnv where
  getInit = view #init

instance HasLogging BenchEnv () where
  getLogging = view #logging

instance HasCommands BenchEnv where
  getCommands = view #commands

instance HasNotifyConfig BenchEnv where
  getNotifyConfig env =
    env ^. #notifyEnv <&> \notifyEnv ->
      MkNotifyConfig
        { action = notifyEnv ^. #action,
          timeout = notifyEnv ^. #timeout
        }

runRegionLoggerBench ::
  Eff (RegionLoggerDynamic () : es) a ->
  Eff es a
runRegionLoggerBench = interpret $ \env -> \case
  LogGlobal _ -> pure ()
  LogRegion {} -> pure ()
  WithRegion _ onRegion -> localSeqUnlift env $ \runner -> runner . onRegion $ ()
  DisplayRegions m -> localSeqUnlift env $ \runner -> runner m

runNotifyBench ::
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyBench = interpret $ \_ -> \case
  Notify _ -> pure ()

runBench :: List String -> IO ()
runBench argList = runEff' $ Environment.withArgs argList $ Env.withEnv $ \env -> do
  consoleQueue <- newTBQueueA 1_000
  let benchEnv =
        MkBenchEnv
          { timeout = env ^. #timeout,
            init = env ^. #init,
            logging =
              MkLogging
                { keyHide = env ^. (#logging % #keyHide),
                  timerFormat = env ^. (#logging % #timerFormat),
                  pollInterval = env ^. (#logging % #pollInterval),
                  cmdNameTrunc = env ^. (#logging % #cmdNameTrunc),
                  cmdLog = env ^. (#logging % #cmdLog),
                  consoleLog = consoleQueue,
                  fileLog = env ^. (#logging % #fileLog)
                },
            commands = env ^. #commands,
            notifyEnv = env ^. #notifyEnv
          }
  runShrun benchEnv (SR.shrun @BenchEnv @())
  where
    runShrun env =
      runReader env
        . evalState @ShrunState mempty
        . P.runTypedProcess
        . HW.runHandleWriterStaticIO
        . HR.runHandleReaderStaticIO
        . runNotifyBench
        . runRegionLoggerBench
        . Time.runTimeDynamicIO

    runEff' =
      runEff
        . Environment.runEnvironment
        . runConcurrent
        . runIORefStaticIO
        . FR.runFileReaderStaticIO
        . FW.runFileWriterStaticIO
        . HW.runHandleWriterStaticIO
        . PR.runPathReaderDynamicIO
        . PW.runPathWriterStaticIO
        . Term.runTerminalDynamicIO
        . OA.runOptparseStaticIO
        . DBus.runDBusDynamicIO
