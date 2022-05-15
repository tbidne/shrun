{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'GoodMockShell' type.
module Integration.MockShell.GoodMockShell
  ( GoodMockShell (..),
    runGoodMockShell,
  )
where

import Data.HashMap.Strict qualified as Map
import Integration.MockEnv (MockEnv)
import Integration.MockShell.MockShellBase
  ( MockShellBase,
    runMockShellBase,
  )
import Integration.Prelude
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Logging.RegionLogger (RegionLogger (..))

-- | 'GoodMockShell' is intended to test a \"Happy path\" run of
-- 'ShellRun.runShell'.
type GoodMockShell :: Type -> Type
newtype GoodMockShell a = MkGoodMockShell (MockShellBase a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter (List Text),
      RegionLogger
    )
    via MockShellBase

runGoodMockShell :: GoodMockShell a -> MockEnv -> (a, List Text)
runGoodMockShell (MkGoodMockShell rdr) = runMockShellBase rdr

instance MonadShell GoodMockShell where
  getDefaultDir = pure "config"
  legendPathToMap "config/shell-run.legend" = pure $ Right $ Map.singleton "def-key" "def-val"
  legendPathToMap _ = pure $ Right mp
    where
      mp =
        Map.fromList
          [ ("cmd1", "command 1"),
            ("cmd2", "command 2"),
            ("both", "cmd1,,cmd2")
          ]

  runCommands = tell . NESeq.toList . fmap (view #command)

instance Show a => Show (GoodMockShell a) where
  show x = "MkGoodMockShell " <> show x