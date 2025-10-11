module Bench.Prelude
  ( BenchEnv (..),
    runBench,
  )
where

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
    HasNotifyConfig,
    HasTimeout,
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
import Shrun.Notify.MonadNotify (MonadNotify (notify))
import Shrun.Prelude
import Shrun.ShellT (ShellT)
import System.Environment qualified as SysEnv

newtype BenchEnv = MkBenchEnv
  {unCoreEnv :: Env ()}
  deriving
    ( HasAnyError,
      HasCommandLogging,
      HasCommands,
      HasCommonLogging,
      HasFileLogging,
      HasInit,
      HasNotifyConfig,
      HasTimeout
    )
    via (Env ())

instance HasConsoleLogging BenchEnv () where
  getConsoleLogging = getConsoleLogging . (.unCoreEnv)

instance MonadRegionLogger (ShellT BenchEnv IO) where
  type Region (ShellT BenchEnv IO) = ()

  logGlobal _ = pure ()

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

  regionList = atomically $ newTMVar []

instance MonadNotify (ShellT BenchEnv IO) where
  notify _ = pure Nothing

runBench :: List String -> IO ()
runBench argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    let benchEnv = MkBenchEnv env
    SR.runShellT SR.shrun benchEnv
