-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
--
-- @since 0.1
module Shrun
  ( ShellT,
    runShellT,
    shrun,
  )
where

import Control.Monad.Loops qualified as Loops
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.Relative (formatRelativeTime, formatSeconds)
import Effects.MonadFs (MonadFsWriter (hFlush, hPut))
import Effects.MonadThread as X (MonadThread (microsleep), sleep)
import Effects.MonadTime (MonadTime (..), withTiming)
import Shrun.Configuration.Env.Types
  ( HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Data.Timeout (Timeout (..))
import Shrun.Effects.Process (Process (..), tryTimeCmd)
import Shrun.IO (Stderr (..))
import Shrun.Logging.Formatting qualified as LFormat
import Shrun.Logging.Log qualified as Log
import Shrun.Logging.Queue (LogText (..), LogTextQueue, _MkLogText)
import Shrun.Logging.Queue qualified as Queue
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.Logging.Types (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import Shrun.Prelude
import Shrun.ShellT (ShellT, runShellT)
import Shrun.Utils qualified as Utils
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import UnliftIO.Async qualified as Async

-- | Entry point
--
-- @since 0.1
shrun ::
  ( HasCommands env,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadCallStack m,
    MonadIORef m,
    MonadFsWriter m,
    MonadMask m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m,
    Process m
  ) =>
  m ()
shrun = asks getCommands >>= runCommands

runCommands ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadCallStack m,
    MonadFsWriter m,
    MonadIORef m,
    MonadMask m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTime m,
    MonadTVar m,
    MonadUnliftIO m,
    Process m
  ) =>
  NonEmptySeq Command ->
  m ()
runCommands commands = Regions.displayConsoleRegions $
  Async.withAsync maybePollQueue $ \fileLogger -> do
    (totalTime, result) <- withTiming $ do
      let actions = Async.mapConcurrently_ runCommand commands
          actionsWithTimer = Async.race_ actions (counter commands)
      tryAny actionsWithTimer

    Async.cancel fileLogger

    Regions.withConsoleRegion Linear $ \r -> do
      case result of
        Left ex -> do
          let errMsg =
                T.pack $
                  "Encountered an exception. This is likely not an error in any of the "
                    <> "commands run but rather an error in Shrun itself: "
                    <> displayException ex
              fatalLog =
                MkLog
                  { cmd = Nothing,
                    msg = errMsg,
                    lvl = LevelFatal,
                    mode = LogModeFinish,
                    dest = LogDestBoth
                  }
          Log.putRegionLog r fatalLog
        Right _ -> pure ()

      let totalTimeTxt =
            "Time: "
              <> formatRelativeTime (Utils.timeSpecToRelTime totalTime)
          finalLog =
            MkLog
              { cmd = Nothing,
                msg = T.pack totalTimeTxt,
                lvl = LevelFinished,
                mode = LogModeFinish,
                dest = LogDestBoth
              }

      Log.putRegionLog r finalLog

      fileLog <- asks getFileLogging
      case fileLog of
        Nothing -> pure ()
        Just (h, queue) -> do
          Queue.flushQueue queue >>= traverse_ (logFile h)
          hFlush h
{-# INLINEABLE runCommands #-}

runCommand ::
  ( HasLogging env,
    MonadMask m,
    MonadCallStack m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m,
    MonadUnliftIO m,
    Process m
  ) =>
  Command ->
  m ()
runCommand cmd = do
  disableLogging <- asks getDisableLogging
  cmdLogging <- asks getCmdLogging
  fileLog <- asks getFileLogging

  -- 1.    Logging is disabled at the global level: No logging at all.
  -- 2.    No CmdLogging and no FileLogging: No streaming at all.
  -- 3.    No CmdLogging and FileLogging: Stream (to file) but no console
  --       region.
  -- 3, 4. CmdLogging: Stream and create the region. FileLogging is globally
  --       enabled/disabled, so no need for a separate function. That is,
  --       tryCommandStreamNoRegion and tryCommandStreamRegion handle file
  --       logging automatically.
  let cmdFn = case (cmdLogging, fileLog, disableLogging) of
        (_, _, True) -> tryCmd
        (False, Nothing, _) -> tryCmd
        (False, Just (_, _), _) -> tryCmdStream
        _ -> tryCmdStreamRegion

  cmdResult <- tryTimeCmd cmdFn cmd

  Regions.withConsoleRegion Linear $ \r -> do
    let (msg', lvl', t') = case cmdResult of
          Left (t, MkStderr err) -> (err <> ". ", LevelError, t)
          Right t -> ("", LevelSuccess, t)
    Log.putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = msg' <> "Time: " <> T.pack (formatRelativeTime t'),
          lvl = lvl',
          mode = LogModeFinish,
          dest = LogDestBoth
        }
{-# INLINEABLE runCommand #-}

counter ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadThread m,
    MonadTVar m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  NonEmptySeq Command ->
  m ()
counter cmds = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  microsleep 100_000
  withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- newIORef 0
    Loops.whileM_ (keepRunning cmds r timer timeout) $ do
      elapsed <- do
        sleep 1
        modifyIORef' timer (+ 1)
        readIORef timer
      logCounter r elapsed
{-# INLINEABLE counter #-}

logCounter ::
  ( HasLogging env,
    MonadReader env m,
    MonadTBQueue m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    MonadTime m
  ) =>
  ConsoleRegion ->
  Natural ->
  m ()
logCounter region elapsed = do
  let lg =
        MkLog
          { cmd = Nothing,
            msg = T.pack (formatSeconds elapsed),
            lvl = LevelTimer,
            mode = LogModeSet,
            dest = LogDestConsole
          }
  Log.putRegionLog region lg
{-# INLINEABLE logCounter #-}

keepRunning ::
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIORef m,
    MonadReader env m,
    MonadTBQueue m,
    MonadTime m,
    MonadTVar m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  NonEmptySeq Command ->
  ConsoleRegion ->
  IORef Natural ->
  Maybe Timeout ->
  m Bool
keepRunning allCmds region timer mto = do
  elapsed <- readIORef timer
  if timedOut elapsed mto
    then do
      cmdDisplay <- asks getCmdDisplay
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- readTVarM completedCmdsTVar

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ NESeq.toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LFormat.displayCmd' cmd cmdDisplay : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Log.putRegionLog region $
        MkLog
          { cmd = Nothing,
            msg = "Timed out, cancelling remaining commands: " <> unfinishedCmds,
            lvl = LevelWarn,
            mode = LogModeFinish,
            dest = LogDestBoth
          }
      pure False
    else pure True
{-# INLINEABLE keepRunning #-}

timedOut :: Natural -> Maybe Timeout -> Bool
timedOut _ Nothing = False
timedOut timer (Just (MkTimeout t)) = timer > t
{-# INLINEABLE timedOut #-}

maybePollQueue ::
  ( HasLogging env,
    MonadFsWriter m,
    MonadReader env m,
    MonadTBQueue m
  ) =>
  m ()
maybePollQueue = do
  fileLog <- asks getFileLogging
  case fileLog of
    Nothing -> pure ()
    Just (h, queue) -> writeQueueToFile h queue
{-# INLINEABLE maybePollQueue #-}

writeQueueToFile ::
  ( MonadFsWriter m,
    MonadTBQueue m
  ) =>
  Handle ->
  LogTextQueue ->
  m void
writeQueueToFile h queue = forever $ Queue.readQueue queue >>= traverse_ (logFile h)
{-# INLINEABLE writeQueueToFile #-}

logFile :: MonadFsWriter m => Handle -> LogText -> m ()
logFile h = hPut h . TEnc.encodeUtf8 . view _MkLogText
{-# INLINEABLE logFile #-}
