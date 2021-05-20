{-# LANGUAGE ImportQualifiedPost #-}

module MockShell.NoLegendMockShell (NoLegendMockShell (..)) where

import Data.Text (Text)
import MockShell.MockShellBase (MockShellBase (..))
import ShellRun.Class.MonadLogger (MonadLogger (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Types.NonNegative qualified as NN

newtype NoLegendMockShell a = MkNoLegendMockShell (MockShellBase a)
  deriving (Show, Semigroup, Monoid) via MockShellBase a
  deriving (Functor, Applicative, Monad) via MockShellBase

instance MonadShell NoLegendMockShell where
  parseArgs :: NoLegendMockShell Args
  parseArgs = pure $ MkArgs legendPath timeout commands
    where
      legendPath = Nothing
      timeout = Just $ NN.unsafeNonNegative 5
      commands = ["cmd1", "cmd2"]

  legendPathToMap :: Text -> NoLegendMockShell (Either LegendErr LegendMap)
  legendPathToMap _ = pure $ Left $ ParseErr "Bad key"

  runCommands :: [Command] -> Maybe NonNegative -> NoLegendMockShell ()
  runCommands commands _ = foldr f (MkNoLegendMockShell (MkMockShellBase () [])) commands
    where
      f (MkCommand cmd) acc = acc <> MkNoLegendMockShell (MkMockShellBase () [cmd])

instance MonadLogger NoLegendMockShell where
  logNoLine :: Text -> NoLegendMockShell ()
  logNoLine t = MkNoLegendMockShell $ MkMockShellBase () [t]

  logLine :: Text -> NoLegendMockShell ()
  logLine t = MkNoLegendMockShell $ MkMockShellBase () [t <> "\n"]
