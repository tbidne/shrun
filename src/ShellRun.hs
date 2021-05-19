{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun
  ( runShell,
  )
where

import Data.Text qualified as T
import ShellRun.Class.MonadLogger (MonadLogger)
import ShellRun.Class.MonadLogger qualified as ML
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Types.Args (Args (..))
import ShellRun.Types.Command (Command (..))

-- TODO: improve this
runShell :: (MonadLogger m, MonadShell m) => m ()
runShell = do
  MkArgs {legend, timeout, commands} <- parseArgs
  maybeCommands <- case legend of
    Just path -> do
      legendMap <- legendPathToMap path
      pure $ case legendMap of
        Right mp -> Right $ ParseCommands.translateCommands mp commands
        Left err -> Left err
    Nothing -> pure $ Right $ fmap MkCommand commands

  case maybeCommands of
    Right cmds -> runCommands cmds timeout
    Left err -> ML.logError $ T.pack $ show err
