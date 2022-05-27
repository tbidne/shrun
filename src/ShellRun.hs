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
import Data.Time.Relative (formatSeconds)
import ShellRun.Command (Command (..))
import ShellRun.Command qualified as Command
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Effects.MonadFSReader (MonadFSReader (..))
import ShellRun.Env
  ( CmdLogging (..),
    HasCommands (..),
    HasCompletedCmds (..),
    HasLegend (..),
    HasLogging (..),
    HasTimeout (..),
  )
import ShellRun.IO (Stderr (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Legend (LegendErr (..))
import ShellRun.Legend qualified as Legend
import ShellRun.Logging.Formatting qualified as LFormat
import ShellRun.Logging.Log qualified as Log
import ShellRun.Logging.Queue (LogText (..), LogTextQueue)
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Logging.Types (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import ShellRun.Prelude
import ShellRun.ShellT (ShellT, runShellT)
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import System.FilePath ((</>))
import UnliftIO.Async qualified as Async

-- | `runShell` is the entry point for running shell commands i.e.
-- `MonadShell` instances.
--
-- @since 0.1
runShell ::
  ( HasCommands env,
    HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    HasLegend env,
    MonadFSReader m,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  m ()
runShell = do
  legendMap <- asks getLegend
  cmds <- asks getCommands
  parsedCommands <- maybePathToCommands legendMap cmds
  runCommandsOrLogErr parsedCommands
{-# INLINEABLE runShell #-}

maybePathToCommands ::
  ( HasLogging env,
    MonadFSReader m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m
  ) =>
  FilePathDefault ->
  NonEmptySeq Text ->
  m (Either LegendErr (NonEmptySeq Command))
maybePathToCommands FPNone cmds = pure $ Right $ fmap (MkCommand Nothing) cmds
maybePathToCommands FPDefault cmds = do
  -- Search for the default legend
  defPath <- (</> "shell-run.legend") <$> getXdgConfig "shell-run"
  eMap <- Legend.legendPathToMap defPath
  case eMap of
    Left le -> case le of
      -- Because we search for the default legend file by default, we do not
      -- want to error if we do not find it (user may not have created it).
      -- Other errors are reported.
      FileErr _ -> do
        Log.putLog $
          MkLog
            { cmd = Nothing,
              msg = "No legend file found at: " <> showt defPath,
              lvl = Info,
              mode = Append,
              dest = LogBoth
            }
        pure $ Right $ fmap (MkCommand Nothing) cmds
      other -> pure $ Left other
    Right lMap -> pure $ Command.translateCommands lMap cmds
maybePathToCommands (FPManual path) cmds = do
  -- Manually specified a legend file, all errors are fair game.
  lMap <- Legend.legendPathToMap path
  pure $ lMap >>= (`Command.translateCommands` cmds)
{-# INLINEABLE maybePathToCommands #-}

runCommandsOrLogErr ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Either LegendErr (NonEmptySeq Command) ->
  m ()
runCommandsOrLogErr (Right cmds) = runCommands cmds
runCommandsOrLogErr (Left err) = Log.putLog log
  where
    errTxt = "Error parsing legend file: " <> showt err
    log =
      MkLog
        { cmd = Nothing,
          msg = errTxt,
          lvl = Fatal,
          mode = Append,
          dest = LogBoth
        }
{-# INLINEABLE runCommandsOrLogErr #-}

runCommands ::
  forall m env.
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  NonEmptySeq Command ->
  m ()
runCommands commands = Regions.displayConsoleRegions $
  Async.withAsync maybePollQueue $ \fileLogger -> do
    start <- liftIO $ C.getTime Monotonic
    let actions = Async.mapConcurrently_ runCommand commands
        actionsWithTimer = Async.race_ actions (counter commands)

    result <- tryAny actionsWithTimer

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

      end <- liftIO $ C.getTime Monotonic
      let totalTime = U.diffTime start end
          totalTimeTxt = "Finished! Total time elapsed: " <> formatSeconds totalTime
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
  ( HasCompletedCmds env,
    HasLogging env,
    MonadMask m,
    MonadReader env m,
    MonadUnliftIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  Command ->
  m ()
runCommand cmd = do
  globalLogging <- asks getGlobalLogging
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
  res <- case (cmdLogging, fileLogging, globalLogging) of
    (_, _, False) -> ShIO.tryTimeSh cmd
    (Disabled, Nothing, _) -> ShIO.tryTimeSh cmd
    (Disabled, Just (_, _), _) -> ShIO.tryTimeShStreamNoRegion cmd
    _ -> ShIO.tryTimeShStreamRegion cmd

  Regions.withConsoleRegion Linear $ \r -> do
    let (msg', lvl', t') = case res of
          Left (t, MkStderr err) -> (err, Error, t)
          Right t -> ("Success", InfoSuccess, t)
    Log.putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = msg' <> ". Time elapsed: " <> T.pack (formatSeconds t'),
          lvl = lvl',
          mode = Finish,
          dest = LogBoth
        }
{-# INLINEABLE runCommand #-}

counter ::
  ( HasCompletedCmds env,
    HasLogging env,
    HasTimeout env,
    MonadMask m,
    MonadReader env m,
    MonadIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  NonEmptySeq Command ->
  m ()
counter cmds = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  liftIO $ threadDelay 100_000
  Regions.withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- liftIO $ newIORef 0
    Loops.whileM_ (keepRunning cmds r timer timeout) $ do
      elapsed <- liftIO $ do
        threadDelay 1_000_000
        modifyIORef' timer (+ 1)
        readIORef timer
      logCounter r elapsed
{-# INLINEABLE counter #-}

logCounter ::
  ( HasLogging env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion
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
  ( HasCompletedCmds env,
    HasLogging env,
    MonadIO m,
    MonadReader env m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  NonEmptySeq Command ->
  ConsoleRegion ->
  IORef Natural ->
  Timeout ->
  m Bool
keepRunning allCmds region timer mto = do
  elapsed <- liftIO $ readIORef timer
  if timedOut elapsed mto
    then do
      cmdDisplay <- asks getCmdDisplay
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- liftIO $ readTVarIO completedCmdsTVar

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

timedOut :: Natural -> Timeout -> Bool
timedOut _ (MkTimeout PPosInf) = False
timedOut timer (MkTimeout (PFin t)) = timer > t
{-# INLINEABLE timedOut #-}

maybePollQueue :: (HasLogging env, MonadIO m, MonadReader env m) => m ()
maybePollQueue = do
  fileLogging <- asks getFileLogging
  case fileLogging of
    Nothing -> pure ()
    Just (fp, queue) -> writeQueueToFile fp queue
{-# INLINEABLE maybePollQueue #-}

writeQueueToFile :: MonadIO m => FilePath -> LogTextQueue -> m void
writeQueueToFile fp queue = forever $ Queue.readQueue queue >>= traverse_ (logFile fp)
{-# INLINEABLE writeQueueToFile #-}

logFile :: MonadIO m => FilePath -> LogText -> m ()
logFile fp = liftIO . appendFileUtf8 fp . view #unLogText
{-# INLINEABLE logFile #-}
