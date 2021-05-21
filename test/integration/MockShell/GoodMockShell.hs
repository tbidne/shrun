{-# LANGUAGE ImportQualifiedPost #-}

module MockShell.GoodMockShell (GoodMockShell (..)) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Types.NonNegative qualified as NN

newtype GoodMockShell a = MkGoodMockShell (MockShellBase a)
  deriving (Show, Semigroup, Monoid) via MockShellBase a
  deriving (Functor, Applicative, Monad) via MockShellBase

instance MonadShell GoodMockShell where
  parseArgs :: GoodMockShell Args
  parseArgs = pure $ MkArgs legendPath timeout commands
    where
      legendPath = Just "legend.txt"
      timeout = Just $ NN.unsafeNonNegative 5
      commands = ["both", "echo hi"]

  legendPathToMap :: Text -> GoodMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Right mp
    where
      mp =
        Map.fromList
          [ ("cmd1", "command 1"),
            ("cmd2", "command 2"),
            ("both", "cmd1,,cmd2")
          ]

  runCommands :: [Command] -> Maybe NonNegative -> GoodMockShell ()
  runCommands commands _ = foldr f (MkGoodMockShell (MkMockShellBase () [])) commands
    where
      f (MkCommand cmd) acc = acc <> MkGoodMockShell (MkMockShellBase () [cmd])

instance MonadLogger GoodMockShell where
  logNoLine :: Text -> GoodMockShell ()
  logNoLine t = MkGoodMockShell $ MkMockShellBase () [t]

  logLine :: Text -> GoodMockShell ()
  logLine t = MkGoodMockShell $ MkMockShellBase () [t <> "\n"]
