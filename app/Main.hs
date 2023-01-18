module Main (main) where

import Effects.MonadCallStack (AnnotatedException (..))
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Prelude
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex ->
    -- NOTE: Seems redundant, but sadly it isn't.
    --
    -- doNothingOnSuccess is solely used to stop ExitSuccess from poisoning the
    -- error code because GHC determines exit success/failure based on the
    -- presence of an uncaught exception. However, we still need to rethrow
    -- the failure.
    --
    -- Moreover, we don't really want to print a CallStack for ExitFailure,
    -- as we are throwing that whenever a subcommand fails, and the CallStack
    -- is just unhelpful noise.
    case fromException ex of
      -- should be impossible due to doNothingOnSuccess...
      Just (AnnotatedException _ ExitSuccess) -> pure ()
      -- for subcommand failures
      Just (AnnotatedException _ (ExitFailure _)) -> pure ()
      Nothing -> putStrLn $ displayCallStack ex

  makeEnvAndShrun `catch` doNothingOnSuccess
  where
    -- We need to catch ExitCode so that optparse applicative's --help
    -- does not set the error code to failure...but then we need to rethrow
    -- failures.
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwIO ex
