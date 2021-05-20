{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun
  ( runShell,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Class.MonadLogger (MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr)
import ShellRun.Types.NonNegative (NonNegative)

runShell :: (MonadLogger m, MonadShell m) => m ()
runShell = do
  MkArgs {legend, timeout, commands} <- parseArgs
  parsedCommands <- maybePathToCommands legend commands
  runCommandsOrLogErr parsedCommands timeout

maybePathToCommands :: MonadShell m => Maybe Text -> [Text] -> m (Either LegendErr [Command])
maybePathToCommands Nothing commands = pure $ Right $ fmap MkCommand commands
maybePathToCommands (Just path) commands = do
  lMap <- legendPathToMap path
  pure $ fmap (`ParseCommands.translateCommands` commands) lMap

runCommandsOrLogErr :: (MonadLogger m, MonadShell m) => Either LegendErr [Command] -> Maybe NonNegative -> m ()
runCommandsOrLogErr (Right cmds) timeout = runCommands cmds timeout
runCommandsOrLogErr (Left err) _ = ML.logError $ T.pack $ show err