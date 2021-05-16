module Main (main) where

import ShellRunner qualified as Sh
import ShellRunner.Parsing (Args (..))
import ShellRunner.Parsing qualified as ShParse

main :: IO ()
main = do
  MkArgs {timeout, commands} <- ShParse.runParser
  Sh.runCommands commands timeout