{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.BadLegendMockShell
  ( BadLegendMockShell (..),
    runBadLegendMockShell,
  )
where

import Integration.IntEnv (IntEnv)
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Effects.MonadProcRunner (MonadProcRunner (..))
import ShellRun.Effects.MonadTime (MonadTime (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.ShellT (ShellT, runShellT)

-- | 'BadLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the path to the legend file is bad.
type BadLegendMockShell :: Type -> Type
newtype BadLegendMockShell a = MkBadLegendMockShell (ShellT IntEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadProcRunner,
      MonadReader IntEnv,
      MonadThrow,
      MonadTime,
      MonadUnliftIO,
      RegionLogger
    )
    via ShellT IntEnv IO

runBadLegendMockShell :: BadLegendMockShell a -> IntEnv -> IO a
runBadLegendMockShell (MkBadLegendMockShell rdr) = runShellT rdr

instance MonadFSReader BadLegendMockShell where
  getXdgConfig _ = pure "config"
  readFile _ = throwString "File not found"
