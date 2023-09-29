module Main (main) where

import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.IORef.Static qualified as IORef
import Effectful.Optparse.Static qualified as Optparse
import Effectful.Process.Typed qualified as P
import Effectful.Terminal.Dynamic qualified as Term
import Effectful.Time.Dynamic qualified as Time
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Configuration.Env.Types (ShrunState)
import Shrun.Logging.RegionLogger (runRegionLoggerDynamicIO)
import Shrun.Notify.AppleScript (runAppleScriptDynamicIO)
import Shrun.Notify.DBus (runDBusDynamicIO)
import Shrun.Notify.NotifySend (runNotifySendDynamicIO)
import Shrun.Prelude
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn $ displayException ex

  runShrun makeEnvAndShrun `catch` doNothingOnSuccess
  where
    -- We need to catchCS ExitCode so that optparse applicative's --help
    -- does not set the error code to failure...but then we need to rethrow
    -- failures.
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwM ex

    runShrun =
      runEff
        . runConcurrent
        . evalState @ShrunState mempty
        . P.runTypedProcess
        . IORef.runIORefStaticIO
        . Optparse.runOptparseStaticIO
        . Time.runTimeDynamicIO
        . FR.runFileReaderStaticIO
        . FW.runFileWriterStaticIO
        . HR.runHandleReaderStaticIO
        . HW.runHandleWriterStaticIO
        . PR.runPathReaderDynamicIO
        . PW.runPathWriterStaticIO
        . Term.runTerminalDynamicIO
        . runRegionLoggerDynamicIO
        . runAppleScriptDynamicIO
        . runDBusDynamicIO
        . runNotifySendDynamicIO
