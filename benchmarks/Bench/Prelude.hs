{-# LANGUAGE UndecidableInstances #-}

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
    HasNotifyConfig (getNotifyConfig),
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
import Shrun.Prelude
import Shrun.ShellT (ShellT)
import System.Environment qualified as SysEnv

newtype BenchEnv = MkBenchEnv
  {unCoreEnv :: Env NotifyEnv ()}
  deriving
    ( HasAnyError,
      HasCommandLogging,
      HasCommands,
      HasCommonLogging,
      HasFileLogging,
      HasInit,
      HasTimeout
    )
    via (Env NotifyEnv ())

instance HasConsoleLogging BenchEnv () where
  getConsoleLogging = getConsoleLogging . (.unCoreEnv)

instance HasNotifyConfig BenchEnv NotifyEnv where
  getNotifyConfig = getNotifyConfig . (.unCoreEnv)

instance MonadRegionLogger (ShellT BenchEnv IO) where
  type Region (ShellT BenchEnv IO) = ()

  logGlobal _ = pure ()

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id

  regionList = atomically $ newTMVar []

instance {-# OVERLAPS #-} MonadNotify (ShellT BenchEnv IO) where
  -- The indirect definition here rather than e.g. NotifyEnv is due to
  -- a conflicting def error, because of the overlapping instance.
  type NotifyEnvF (ShellT BenchEnv IO) = NotifyEnvF (ReaderT BenchEnv IO)

  initNotifyEnv _ = pure $ error "unimplemented"
  notify _ _ = pure ()

runBench :: List String -> IO ()
runBench argList = do
  SysEnv.withArgs argList $ Env.withEnv $ \env -> do
    let benchEnv = MkBenchEnv env
    SR.runShellT SR.shrun benchEnv
