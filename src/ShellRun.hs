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
import ShellRun.Types.Args (Args (..), NativeLog (..))
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr)
import ShellRun.Types.NonNegative (NonNegative)

runShell :: (MonadLogger m, MonadShell m) => m ()
runShell = do
  MkArgs {legend, timeout, nativeLog, commands} <- parseArgs
  parsedCommands <- maybePathToCommands legend commands
  runCommandsOrLogErr parsedCommands timeout nativeLog

maybePathToCommands :: MonadShell m => Maybe Text -> [Text] -> m (Either LegendErr [Command])
maybePathToCommands Nothing commands = pure $ Right $ fmap MkCommand commands
maybePathToCommands (Just path) commands = do
  lMap <- legendPathToMap path
  pure $ lMap >>= (`ParseCommands.translateCommands` commands)

runCommandsOrLogErr :: (MonadLogger m, MonadShell m) => Either LegendErr [Command] -> Maybe NonNegative -> NativeLog -> m ()
runCommandsOrLogErr (Right cmds) timeout nativeLog = runCommands cmds timeout nativeLog
runCommandsOrLogErr (Left err) _ _ = ML.logError errTxt
  where
    errTxt = "Error parsing legend file: " <> T.pack (show err)