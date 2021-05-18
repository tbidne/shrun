module Main (main) where

import ShellRunner qualified as Sh
import ShellRunner.Parsing.Args (Args (..))
import ShellRunner.Parsing.Args qualified as ParseArgs
import ShellRunner.Parsing.Legend qualified as ParseLegend
import ShellRunner.Parsing.Commands qualified as ParseCommands
import ShellRunner.Types.Command (Command (..))

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
    Left err -> putStrLn $ show err