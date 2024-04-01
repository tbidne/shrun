{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Bench.Prelude
  ( BenchEnv (..),
    runBench,
  )
where

import Shrun qualified as SR
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Timeout (Timeout)
import Shrun.Env qualified as Env
import Shrun.Env.Types
  ( HasAnyError (getAnyError),
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
    NotifyEnv,
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
import Shrun.Notify.MonadNotify (MonadNotify (notify))
import Shrun.Notify.Types (NotifyConfig (MkNotifyConfig, action, timeout))
import Shrun.Prelude
import Shrun.ShellT (ShellT)
import System.Environment qualified as SysEnv

data BenchEnv = MkBenchEnv
  { timeout :: Maybe Timeout,
    init :: Maybe Text,
    logging :: Logging (),
    completedCmds :: TVar (Seq CommandP1),
    commands :: NESeq CommandP1,
    notifyEnv :: Maybe NotifyEnv,
    anyError :: TVar Bool
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
  getCompletedCmds = view #completedCmds

instance HasAnyError BenchEnv where
  getAnyError = view #anyError

instance HasNotifyConfig BenchEnv where
  getNotifyConfig env =
    (env ^. #notifyEnv) <&> \notifyEnv ->
      MkNotifyConfig
        { action = notifyEnv ^. #action,
          timeout = notifyEnv ^. #timeout
        }

instance MonadRegionLogger (ShellT BenchEnv IO) where
  type Region (ShellT BenchEnv IO) = ()

  logGlobal _ = pure ()

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

instance MonadNotify (ShellT BenchEnv IO) where
  notify _ = pure Nothing

runBench :: List String -> IO ()
runBench argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
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
                    cmdLogReadSize = env ^. (#logging % #cmdLogReadSize),
                    cmdLog = env ^. (#logging % #cmdLog),
                    consoleLog = consoleQueue,
                    fileLog = env ^. (#logging % #fileLog)
                  },
              completedCmds = env ^. #completedCmds,
              anyError = env ^. #anyError,
              commands = env ^. #commands,
              notifyEnv = env ^. #notifyEnv
            }
    SR.runShellT SR.shrun benchEnv
