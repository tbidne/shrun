-- | This module is the entry point to the @Shell-Run@ library used by
-- the @Shell-Run@ executable.
--
-- @since 0.1
module ShellRun
  ( ShellT,
    runShellT,
    runShell,
  )
where

import Control.Monad.Loops qualified as Loops
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time.Relative (formatRelativeTime, formatSeconds)
import ShellRun.Configuration.Env
  ( CmdLogging (..),
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Effects.Atomic (Atomic (..))
import ShellRun.Effects.FileSystemWriter (FileSystemWriter (..))
import ShellRun.Effects.Terminal (Terminal (..))
import ShellRun.Effects.TimedProcess (TimedProcess (..))
import ShellRun.Effects.Timing (Timing (..), withTiming)
import ShellRun.IO (Stderr (..))
import ShellRun.Logging.Formatting qualified as LFormat
import ShellRun.Logging.Log qualified as Log
import ShellRun.Logging.Queue (LogText (..), LogTextQueue, _LogText)
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Logging.Types (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import ShellRun.Prelude
import ShellRun.ShellT (ShellT, runShellT)
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import UnliftIO.Async qualified as Async

-- | Entry point
--
-- @since 0.1
runShell ::
  ( Atomic m,
    FileSystemWriter m,
    HasCommands env,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m,
    TimedProcess m,
    Timing m
  ) =>
  m ()
runShell = asks getCommands >>= runCommands

runCommands ::
  forall m env.
  ( Atomic m,
    FileSystemWriter m,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m,
    TimedProcess m,
    Timing m
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
                    <> "commands run but rather an error in ShellRun itself: "
                    <> displayException ex
              fatalLog =
                MkLog
                  { cmd = Nothing,
                    msg = errMsg,
                    lvl = Fatal,
                    mode = Finish,
                    dest = LogBoth
                  }
          Log.putRegionLog r fatalLog
        Right _ -> pure ()

      let totalTimeTxt = "Finished! Total time elapsed: " <> formatRelativeTime totalTime
          finalLog =
            MkLog
              { cmd = Nothing,
                msg = T.pack totalTimeTxt,
                lvl = InfoBlue,
                mode = Finish,
                dest = LogBoth
              }

      Log.putRegionLog r finalLog

      fileLogging <- asks getFileLogging
      case fileLogging of
        Nothing -> pure ()
        Just (fp, queue) -> Queue.flushQueue queue >>= traverse_ (logFile fp)
{-# INLINEABLE runCommands #-}

runCommand ::
  ( Atomic m,
    HasLogging env,
    MonadMask m,
    TimedProcess m,
    MonadReader env m,
    MonadUnliftIO m,
    Timing m
  ) =>
  Command ->
  m ()
runCommand cmd = do
  disableLogging <- asks getDisableLogging
  cmdLogging <- asks getCmdLogging
  fileLogging <- asks getFileLogging

  -- 1.    Logging is disabled at the global level: No logging at all.
  -- 2.    No CmdLogging and no FileLogging: No streaming at all.
  -- 3.    No CmdLogging and FileLogging: Stream (to file) but no console
  --       region.
  -- 3, 4. CmdLogging: Stream and create the region. FileLogging is globally
  --       enabled/disabled, so no need for a separate function. That is,
  --       tryTimeShStreamNoRegion and tryTimeShStreamRegion handle file
  --       logging automatically.
  res <- case (cmdLogging, fileLogging, disableLogging) of
    (_, _, True) -> tryTime cmd
    (Disabled, Nothing, _) -> tryTime cmd
    (Disabled, Just (_, _), _) -> tryTimeStream cmd
    _ -> tryTimeStreamRegion cmd

  Regions.withConsoleRegion Linear $ \r -> do
    let (msg', lvl', t') = case res of
          Left (t, MkStderr err) -> (err, Error, t)
          Right t -> ("Success", InfoSuccess, t)
    Log.putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = msg' <> ". Time elapsed: " <> T.pack (formatRelativeTime t'),
          lvl = lvl',
          mode = Finish,
          dest = LogBoth
        }
{-# INLINEABLE runCommand #-}

counter ::
  ( Atomic m,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    Terminal m,
    Timing m
  ) =>
  NonEmptySeq Command ->
  m ()
counter cmds = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  sleep 100_000
  withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- newIORef 0
    Loops.whileM_ (keepRunning cmds r timer timeout) $ do
      elapsed <- do
        sleep 1_000_000
        modifyIORef' timer (+ 1)
        readIORef timer
      logCounter r elapsed
{-# INLINEABLE counter #-}

logCounter ::
  ( Atomic m,
    HasLogging env,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    Timing m
  ) =>
  ConsoleRegion ->
  Natural ->
  m ()
logCounter region elapsed = do
  let lg =
        MkLog
          { cmd = Nothing,
            msg = "Running time: " <> T.pack (formatSeconds elapsed),
            lvl = InfoCyan,
            mode = Set,
            dest = LogConsole
          }
  Log.putRegionLog region lg
{-# INLINEABLE logCounter #-}

keepRunning ::
  ( Atomic m,
    HasCompletedCmds env,
    HasLogging env,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion,
    Timing m
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
      completedCmds <- readTVarIO completedCmdsTVar

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ NESeq.toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LFormat.displayCmd' cmd cmdDisplay : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      Log.putRegionLog region $
        MkLog
          { cmd = Nothing,
            msg = "Timed out, cancelling remaining commands: " <> unfinishedCmds,
            lvl = Warn,
            mode = Finish,
            dest = LogBoth
          }
      pure False
    else pure True
{-# INLINEABLE keepRunning #-}

timedOut :: Natural -> Maybe Timeout -> Bool
timedOut _ Nothing = False
timedOut timer (Just (MkTimeout t)) = timer > t
{-# INLINEABLE timedOut #-}

maybePollQueue :: (Atomic m, FileSystemWriter m, HasLogging env, MonadReader env m) => m ()
maybePollQueue = do
  fileLogging <- asks getFileLogging
  case fileLogging of
    Nothing -> pure ()
    Just (fp, queue) -> writeQueueToFile fp queue
{-# INLINEABLE maybePollQueue #-}

writeQueueToFile :: (Atomic m, FileSystemWriter m) => FilePath -> LogTextQueue -> m void
writeQueueToFile fp queue = forever $ Queue.readQueue queue >>= traverse_ (logFile fp)
{-# INLINEABLE writeQueueToFile #-}

logFile :: FileSystemWriter m => FilePath -> LogText -> m ()
logFile fp = appendFile fp . view _LogText
{-# INLINEABLE logFile #-}
