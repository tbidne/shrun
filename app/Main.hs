{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import ShellRun qualified as Sh
import ShellRun.Parsing.Args (Args (..))
import ShellRun.Parsing.Args qualified as ParseArgs
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Parsing.Legend qualified as ParseLegend
import ShellRun.Types.Command (Command (..))

main :: IO ()
main = do
  MkArgs {legend, timeout, commands} <- ParseArgs.runParser
  maybeCommands <- case legend of
    Just path -> do
      legendMap <- ParseLegend.legendPathToMap path
      pure $ case legendMap of
        Right mp -> Right $ ParseCommands.translateCommands mp commands
        Left err -> Left err
    Nothing -> pure $ Right $ fmap MkCommand commands

  case maybeCommands of
    Right cmds -> Sh.runCommands cmds timeout
    Left err -> print err