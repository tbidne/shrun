{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.GoodMockShell
  ( GoodMockShell (..),
    runGoodMockShell,
  )
where

import Data.Text qualified as T
import Integration.IntEnv (IntEnv)
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Effects.MonadProcRunner (MonadProcRunner (..))
import ShellRun.Effects.MonadTime (MonadTime (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.ShellT (ShellT, runShellT)

-- | 'GoodMockShell' is intended to test a \"Happy path\" run of
-- 'ShellRun.runShell'.
type GoodMockShell :: Type -> Type
newtype GoodMockShell a = MkGoodMockShell (ShellT IntEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadProcRunner,
      MonadReader IntEnv,
      MonadTime,
      MonadThrow,
      MonadUnliftIO,
      RegionLogger
    )
    via ShellT IntEnv IO

runGoodMockShell :: GoodMockShell a -> IntEnv -> IO a
runGoodMockShell (MkGoodMockShell rdr) = runShellT rdr

instance MonadFSReader GoodMockShell where
  getXdgConfig _ = pure "config"
  readFile "config/shell-run.legend" = pure "def-key=def-val"
  readFile _ = do
    pure legendTxt
    where
      legendTxt =
        T.unlines
          [ "cmd1=cmd 1",
            "cmd2=cmd 2",
            "both=cmd1,,cmd2,,echo hi"
          ]
