module Main (main) where

import ShellRun qualified as SR
import ShellRun.Env qualified as Env
import ShellRun.Prelude

main :: IO ()
main = do
  env <- Env.runParser
  SR.runShellT SR.runShell env
