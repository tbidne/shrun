{-# LANGUAGE ImplicitPrelude #-}

module Test.Shrun.Process
  ( -- * Process
    runProcess,
    runProcessArgs,
    runProcessOrDie,
    runProcessOrDieQuiet,
    runProcessTotal,

    -- * Misc
    displayCmd,
  )
where

import Control.Exception (displayException)
import Control.Exception.Utils (throwString, trySync)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.FileSystem.FileReader (OsPath)
import Effects.System.Process qualified as P
import GHC.Stack.Types (HasCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Shrun.Logger qualified as Logger

runProcess :: (HasCallStack) => OsPath -> String -> IO (ExitCode, String, String)
runProcess testDir txt = do
  Logger.putLog testDir $ "Running '" ++ txt ++ "'"
  trySync (P.readCreateProcessWithExitCode (P.shell txt) "runProcess") >>= \case
    Right r -> pure r
    Left err -> pure (ExitFailure 1, "Exception running command: " ++ txt, displayException err)

runProcessArgs :: OsPath -> String -> [String] -> IO (ExitCode, String, String)
runProcessArgs testDir cmd args = do
  Logger.putLog testDir $ "Running '" ++ displayCmd cmd args ++ "'"
  trySync (P.readProcessWithExitCode cmd args "runProcessArgs") >>= \case
    Right r -> pure r
    Left err -> pure (ExitFailure 1, "Exception running command: " ++ cmd, displayException err)

runProcessOrDie :: (HasCallStack) => OsPath -> String -> IO ()
runProcessOrDie testDir txt = do
  (ec, out, err) <- runProcess testDir txt
  case ec of
    ExitSuccess ->
      Logger.putLogLines testDir $
        mconcat
          [ "Process '",
            txt,
            "' succeeded. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]
    ExitFailure _ -> do
      throwString $
        mconcat
          [ "Process '",
            txt,
            "' failed. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]

runProcessOrDieQuiet :: (HasCallStack) => OsPath -> String -> IO Text
runProcessOrDieQuiet testDir txt = do
  (ec, out, err) <- runProcess testDir txt
  case ec of
    ExitSuccess -> pure $ T.pack out
    ExitFailure _ -> do
      throwString $
        mconcat
          [ "Process '",
            txt,
            "' failed. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]

runProcessTotal :: (HasCallStack) => OsPath -> String -> IO ()
runProcessTotal testDir txt = do
  (ec, out, err) <- runProcess testDir txt
  case ec of
    ExitSuccess ->
      Logger.putLogLines testDir $
        mconcat
          [ "Process '",
            txt,
            "' succeeded. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]
    ExitFailure _ -> do
      Logger.putLogLines testDir $
        mconcat
          [ "Process '",
            txt,
            "' failed. Stdout: '",
            out,
            "', stderr: '",
            err,
            "'"
          ]

displayCmd :: String -> [String] -> String
displayCmd cmd args =
  mconcat
    [ "command: '",
      cmd,
      "', args: ",
      show args
    ]
