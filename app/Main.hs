module Main (main) where

import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Prelude
import System.Exit (ExitCode (..), exitFailure)

main :: IO ()
main =
  makeEnvAndShrun
    `catch` doNothingOnSuccess
    `catchAny` printExceptions
  where
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwIO ex
    printExceptions =
      putStrLn . pack . displayException >=> const exitFailure
