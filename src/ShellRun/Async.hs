{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the `runCommands` function used for running a list
--   of commands asynchronously.
module ShellRun.Async
  ( runCommands,
  )
where

import Control.Concurrent qualified as Concurrent
import Control.Exception qualified as Except
import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops qualified as Loops
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Numeric.Algebra (ASemigroup (..))
import Refined (NonNegative, Refined)
import Refined qualified as R
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Env
  ( CommandLogging (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasTimeout (..),
  )
import ShellRun.Data.IO (Stderr (..))
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import UnliftIO qualified
import UnliftIO.Async qualified as UAsync

-- | Runs all commands asynchronously while printing out a running counter.
-- When a command finishes/crashes, stdout is updated with the result. If
-- a haskell exception is encountered in @shell-run@ /itself/, this is
-- considered a fatal error and all threads are killed.
runCommands ::
  ( HasCommandDisplay env,
    HasCommandLogging env,
    HasTimeout env,
    MonadIO m,
    MonadMask m,
    MonadUnliftIO m,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  [Command] ->
  m ()
runCommands commands = Regions.displayConsoleRegions $
  Regions.withConsoleRegion Linear $ \r -> do
    start <- liftIO $ C.getTime Monotonic
    let actions = UAsync.mapConcurrently_ runCommand commands
        actionsWithTimer = UAsync.race_ actions counter

    result :: Either SomeException () <- UnliftIO.withRunInIO $ \runner -> SafeEx.try $ runner actionsWithTimer

    case result of
      Left ex -> do
        let errMsg =
              T.pack $
                "Encountered an exception. This is likely not an error in any of the "
                  <> "commands run but rather an error in ShellRun itself: "
                  <> Except.displayException ex
            fatalLog = MkLog errMsg Fatal Finish
        putRegionLog r fatalLog
      Right _ -> pure ()

    end <- liftIO $ C.getTime Monotonic
    let totalTime = U.diffTime start end
        totalTimeTxt = "Finished! Total time elapsed: " <> U.formatTime totalTime
        finalLog = MkLog totalTimeTxt InfoBlue Finish

    putRegionLog r finalLog

runCommand ::
  ( HasCommandDisplay env,
    HasCommandLogging env,
    MonadMask m,
    MonadReader env m,
    MonadIO m
  ) =>
  Command ->
  m ()
runCommand cmd = Regions.withConsoleRegion Linear $ \r -> do
  commandDisplay <- asks getCommandDisplay
  commandLogging <- asks getCommandLogging

  let shFn = case commandLogging of
        Disabled -> ShIO.tryTimeSh commandDisplay
        Enabled -> ShIO.tryTimeShRegion commandDisplay

  liftIO $ do
    res <- shFn cmd
    let lg = case res of
          Left (t, MkStderr err) ->
            let logTxt =
                  err
                    <> ". Time elapsed: "
                    <> U.formatTime t
             in MkLog logTxt Error Finish
          Right t ->
            let name = U.displayCommand commandDisplay cmd
                logTxt =
                  "Successfully ran `"
                    <> name
                    <> "`. Time elapsed: "
                    <> U.formatTime t
             in MkLog logTxt InfoSuccess Finish
    putRegionLog r lg

counter ::
  ( HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  m ()
counter = Regions.withConsoleRegion Linear $ \r -> do
  timeout <- asks getTimeout
  timer <- liftIO $ IORef.newIORef $$(R.refineTH @NonNegative @Int 0)
  let inc = $$(R.refineTH @NonNegative @Int 1)
  Loops.whileM_ (keepRunning r timer timeout) $ do
    elapsed <- liftIO $ do
      Concurrent.threadDelay 1_000_000
      IORef.modifyIORef' timer (.+. inc)
      IORef.readIORef timer
    logCounter r elapsed

logCounter ::
  ( RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  ConsoleRegion ->
  Refined NonNegative Int ->
  m ()
logCounter r elapsed = do
  let lg =
        MkLog
          { msg = "Running time: " <> U.formatTime elapsed,
            lvl = InfoCyan,
            mode = Set
          }
  putRegionLog r lg

keepRunning ::
  ( MonadIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  ConsoleRegion ->
  IORef (Refined NonNegative Int) ->
  Maybe Timeout ->
  m Bool
keepRunning region timer mto = do
  elapsed <- liftIO $ IORef.readIORef timer
  if timedOut elapsed mto
    then do
      putRegionLog region $ MkLog "Timed out, cancelling remaining tasks." Warn Finish
      pure False
    else pure True

timedOut :: Refined NonNegative Int -> Maybe Timeout -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just (MkTimeout t) -> timer > t
