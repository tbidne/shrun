{-# LANGUAGE ImportQualifiedPost #-}

module MockShell.GoodMockShell (GoodMockShell (..)) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer qualified as MTL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import MockEnv (MockEnv)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

newtype GoodMockShell a = MkGoodMockShell {runGoodMockShell :: MockShellBase a}
  deriving (Functor, Applicative, Monad, MonadReader MockEnv, MonadWriter [Text], MonadLogger)

instance MonadShell GoodMockShell where
  legendPathToMap :: Text -> GoodMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Right mp
    where
      mp =
        Map.fromList
          [ ("cmd1", "command 1"),
            ("cmd2", "command 2"),
            ("both", "cmd1,,cmd2")
          ]

  runCommands :: [Command] -> GoodMockShell ()
  runCommands = MTL.tell . fmap getCommand

instance Show a => Show (GoodMockShell a) where
  show _ = "MkGoodMockShell"
