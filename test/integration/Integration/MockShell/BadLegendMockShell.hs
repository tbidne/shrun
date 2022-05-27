{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.BadLegendMockShell
  ( BadLegendMockShell (..),
    runBadLegendMockShell,
  )
where

import Integration.MockEnv (MockEnv)
import Integration.MockShell.MockShellBase (MockShellBase, runMockShellBase)
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'BadLegendMockShell' is intended to test a run of
-- 'ShellRun.runShell' when the path to the legend file is bad.
type BadLegendMockShell :: Type -> Type
newtype BadLegendMockShell a = MkBadLegendMockShell (MockShellBase a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader MockEnv,
      MonadThrow,
      MonadUnliftIO,
      RegionLogger
    )
    via MockShellBase

runBadLegendMockShell :: BadLegendMockShell a -> MockEnv -> IO a
runBadLegendMockShell (MkBadLegendMockShell rdr) = runMockShellBase rdr

instance MonadFSReader BadLegendMockShell where
  getXdgConfig _ = pure "config"
  readFile _ = throwString "File not found"
