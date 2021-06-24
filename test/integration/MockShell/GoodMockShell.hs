{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the 'GoodMockShell' type.
module MockShell.GoodMockShell (GoodMockShell (..)) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer qualified as MTL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Logging (MonadLogger (..))
import ShellRun.Types.Command (Command (..))

-- | 'GoodMockShell' is intended to test a \"Happy path\" run of
-- 'ShellRun.runShell'.
newtype GoodMockShell a = MkGoodMockShell {runGoodMockShell :: MockShellBase a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader MockEnv,
      MonadWriter [Text],
      MonadLogger
    )
    via MockShellBase

instance MonadShell GoodMockShell where
  legendPathToMap _ = pure $ Right mp
    where
      mp =
        Map.fromList
          [ ("cmd1", "command 1"),
            ("cmd2", "command 2"),
            ("both", "cmd1,,cmd2")
          ]

  runCommands = MTL.tell . fmap command

instance Show a => Show (GoodMockShell a) where
  show _ = "MkGoodMockShell"
