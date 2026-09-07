module Main (main) where

import GHC.Conc (setUncaughtExceptionHandler)
import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Logging.RegionLogger (runRegionLogger)
import Shrun.Prelude hiding (IO)
import Prelude (IO)
import Prelude qualified

main :: IO ()
main = do
  setUncaughtExceptionHandler handleEx

  runShrun $ makeEnvAndShrun @NotifyEnv @ConsoleRegion
  where
    handleEx ex = case fromException ex of
      -- Do not print ExitCode
      Just ExitSuccess -> pure ()
      Just (ExitFailure _) -> pure ()
      Nothing -> case fromException ex of
        -- Do not print term exception, since we handle it elsewhere. It
        -- reaches this point purely to kill the program.
        Just MkTermException -> pure ()
        Nothing -> Prelude.putStrLn $ displayException ex

    runShrun =
      runEff
        . runConcurrent
        . runProcess
        . runPrim
        . runOptparse
        . runTime
        . runFileReader
        . runFileWriter
        . runHandleReader
        . runHandleWriter
        . runPathReader
        . runPathWriter
        . runPosixFiles
        . runPosixSignals
        . runTerminal
        . runRegionLogger
        . runNotify
