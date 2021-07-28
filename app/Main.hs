module Main (main) where

import ShellRun qualified as SR
import ShellRun.Parsing.Env qualified as Env
import ShellRun.Prelude

main :: IO ()
main = do
  env <- Env.runParser
  runReaderT (SR.runShellT SR.runShell) env
