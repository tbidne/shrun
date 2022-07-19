module Main (main) where

import ShellRun qualified as SR
import ShellRun.Configuration.Env (makeEnv)
import ShellRun.Prelude
import System.Exit (ExitCode (..), exitFailure)

main :: IO ()
main =
  run
    `catch` doNothingOnSuccess
    `catchAny` printExceptions
  where
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwIO ex
    printExceptions =
      putStrLn . pack . displayException >=> const exitFailure

run :: IO ()
run = makeEnv >>= SR.runShellT SR.runShell
