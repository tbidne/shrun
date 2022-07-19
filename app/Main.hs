module Main (main) where

import Shrun (runShellT, shrun)
import Shrun.Configuration.Env (makeEnv)
import Shrun.Prelude
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
run = makeEnv >>= runShellT shrun
