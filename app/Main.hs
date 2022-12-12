module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Prelude
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)

  makeEnvAndShrun
    `catch` doNothingOnSuccess
  where
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwIO ex
