{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.GoodMockShell
  ( GoodMockShell (..),
    runGoodMockShell,
  )
where

import Data.Text qualified as T
import Integration.MockEnv (MockEnv)
import Integration.MockShell.MockShellBase
  ( MockShellBase,
    runMockShellBase,
  )
import Integration.Prelude
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'GoodMockShell' is intended to test a \"Happy path\" run of
-- 'ShellRun.runShell'.
type GoodMockShell :: Type -> Type
newtype GoodMockShell a = MkGoodMockShell (MockShellBase a)
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

runGoodMockShell :: GoodMockShell a -> MockEnv -> IO a
runGoodMockShell (MkGoodMockShell rdr) = runMockShellBase rdr

instance MonadFSReader GoodMockShell where
  getXdgConfig _ = pure "config"
  readFile "config/shell-run.legend" = pure "def-key=def-val"
  readFile _ = do
    pure legendTxt
    where
      legendTxt =
        T.unlines
          [ "cmd1=echo 1",
            "cmd2=echo 2",
            "both=sleep 0.5,,cmd1,,cmd2"
          ]
