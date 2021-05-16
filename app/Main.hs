module Main (main) where

import ShellRunner qualified as Sh
import ShellRunner.Types (Command (..))
import ShellRunner.Parsing qualified as ShParse
import ShellRunner.Parsing (Args (..))

main :: IO ()
main = do
  MkArgs{timeout, commands} <- ShParse.runParser
  Sh.runCommands commands timeout