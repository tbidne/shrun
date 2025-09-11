module Main (main) where

import GHC.Conc (setUncaughtExceptionHandler)
import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Prelude hiding (IO)
import Prelude (IO)

main :: IO ()
main = do
  setUncaughtExceptionHandler handleEx

  makeEnvAndShrun @IO @ConsoleRegion
  where
    handleEx ex = case fromException ex of
      -- Do not print ExitCode
      Just ExitSuccess -> pure ()
      Just (ExitFailure _) -> pure ()
      Nothing -> case fromException ex of
        -- Do not print term exception, since we handle it elsewhere. It
        -- reaches this point purely to kill the program.
        Just MkTermException -> pure ()
        Nothing -> putStrLn $ displayException ex
