module Main (main) where

import ShellRun qualified as SR
import ShellRun.Configuration.Env (makeEnv)
import ShellRun.Prelude
import System.Exit (ExitCode)

main :: IO ()
main =
  run
    `catch` doNothingOnSuccess
    `catchAny` printExceptions
  where
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess _ = pure ()
    printExceptions = putStrLn . pack . displayException

run :: IO ()
run = makeEnv >>= SR.runShellT SR.runShell
