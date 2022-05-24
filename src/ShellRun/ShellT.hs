{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'ShellT' monad transformer.
--
-- @since 0.1
module ShellRun.ShellT
  ( ShellT,
    runShellT,
  )
where

import Control.Concurrent qualified as CC
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad qualified as M
import Control.Monad.Catch (MonadMask)
import Control.Monad.Loops qualified as Loops
import Data.HashSet qualified as Set
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Time.Relative (formatSeconds)
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Class.MonadTime (MonadTime (..))
import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env
  ( CmdLogging (..),
    Env (..),
    HasCmdDisplay (..),
    HasCmdLineTrunc (..),
    HasCmdLogging (..),
    HasCmdNameTrunc (..),
    HasCompletedCmds (..),
    HasFileLogging (..),
    HasGlobalLogging (..),
    HasTimeout (..),
  )
import ShellRun.IO (Stderr (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Legend (LegendErr, LegendMap)
import ShellRun.Legend qualified as Legend
import ShellRun.Logging.Formatting qualified as LFormat
import ShellRun.Logging.Log (Log (..), LogDest (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.Queue (LogText (..), LogTextQueue)
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Dir
import UnliftIO.Async qualified as Async

-- | `ShellT` is the main application type that runs shell commands.
--
-- @since 0.1
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT env m a = MkShellT (ReaderT env m a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadCatch,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      MonadMask,
      -- | @since 0.1
      MonadTime,
      -- | @since 0.1
      MonadThrow,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)
  deriving
    ( -- | @since 0.1
      MonadTrans
    )
    via (ReaderT env)

-- | Runs a 'ShellT' with the given @env@.
--
-- @since 0.1
runShellT :: ShellT env m a -> env -> m a
runShellT (MkShellT rdr) = runReaderT rdr
{-# INLINEABLE runShellT #-}

-- | @since 0.1
instance
  ( HasCmdDisplay env,
    HasCmdNameTrunc env,
    HasCmdLineTrunc env,
    HasFileLogging env,
    HasGlobalLogging env,
    MonadIO m
  ) =>
  RegionLogger (ShellT env m)
  where
  type Region (ShellT env m) = ConsoleRegion

  putLog :: Log -> ShellT env m ()
  putLog log = do
    b <- asks getGlobalLogging
    if b
      then do
        maybeSendLogToQueue log
        maybePrintLog (liftIO . putStrLn) log
      else pure ()
  {-# INLINEABLE putLog #-}

  putRegionLog :: ConsoleRegion -> Log -> ShellT env m ()
  putRegionLog region lg@MkLog {mode} = do
    b <- asks getGlobalLogging
    if b
      then do
        let logFn = case mode of
              Set -> Regions.setConsoleRegion
              Append -> Regions.appendConsoleRegion
              Finish -> Regions.finishConsoleRegion

        maybeSendLogToQueue lg
        maybePrintLog (liftIO . logFn region) lg
      else pure ()
  {-# INLINEABLE putRegionLog #-}

maybePrintLog ::
  ( HasCmdDisplay env,
    HasCmdNameTrunc env,
    HasCmdLineTrunc env,
    MonadReader env m
  ) =>
  (Text -> m ()) ->
  Log ->
  m ()
maybePrintLog fn log@MkLog {dest} = do
  case dest of
    LogFile -> pure ()
    _ -> LFormat.formatConsoleLog log >>= fn
{-# INLINEABLE maybePrintLog #-}

-- | @since 0.1
instance
  ( MonadCatch m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  MonadShell (ShellT Env m)
  where
  getDefaultDir :: ShellT Env m FilePath
  getDefaultDir = liftIO $ Dir.getXdgDirectory XdgConfig "shell-run"
  {-# INLINEABLE getDefaultDir #-}

  legendPathToMap :: FilePath -> ShellT Env m (Either LegendErr LegendMap)
  legendPathToMap = liftIO . Legend.legendPathToMap
  {-# INLINEABLE legendPathToMap #-}

  runCommands :: NonEmptySeq Command -> ShellT Env m ()
  runCommands commands = Regions.displayConsoleRegions $
    Async.withAsync maybePollQueue $ \fileLogger -> do
      start <- liftIO $ C.getTime Monotonic
      let actions = Async.mapConcurrently_ runCommand commands
          actionsWithTimer = Async.race_ actions (counter commands)

      result <- try @_ @SomeException @() $
        withRunInIO $ \runInIO -> runInIO actionsWithTimer

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
            putRegionLog r fatalLog
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

        putRegionLog r finalLog

        fileLogging <- asks getFileLogging
        case fileLogging of
          Nothing -> pure ()
          Just (fp, queue) -> Queue.flushQueue queue >>= traverse_ (logFile fp)
  {-# INLINEABLE runCommands #-}

runCommand ::
  ( HasCmdDisplay env,
    HasCmdLogging env,
    HasCmdNameTrunc env,
    HasCmdLineTrunc env,
    HasCompletedCmds env,
    HasFileLogging env,
    HasGlobalLogging env,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  Command ->
  ShellT env m ()
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
    putRegionLog r $
      MkLog
        { cmd = Just cmd,
          msg = msg' <> ". Time elapsed: " <> T.pack (formatSeconds t'),
          lvl = lvl',
          mode = Finish,
          dest = LogBoth
        }
{-# INLINEABLE runCommand #-}

counter ::
  ( HasCmdDisplay env,
    HasCompletedCmds env,
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
  liftIO $ CC.threadDelay 100_000
  Regions.withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- liftIO $ IORef.newIORef 0
    Loops.whileM_ (keepRunning cmds r timer timeout) $ do
      elapsed <- liftIO $ do
        CC.threadDelay 1_000_000
        IORef.modifyIORef' timer (+ 1)
        IORef.readIORef timer
      logCounter r elapsed
{-# INLINEABLE counter #-}

logCounter ::
  ( RegionLogger m,
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
  putRegionLog region lg
{-# INLINEABLE logCounter #-}

keepRunning ::
  ( HasCmdDisplay env,
    HasCompletedCmds env,
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
  elapsed <- liftIO $ IORef.readIORef timer
  if timedOut elapsed mto
    then do
      cmdDisplay <- asks getCmdDisplay
      completedCmdsTVar <- asks getCompletedCmds
      completedCmds <- liftIO $ TVar.readTVarIO completedCmdsTVar

      let completedCmdsSet = Set.fromList $ toList completedCmds
          allCmdsSet = Set.fromList $ NESeq.toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCmdsSet
          toTxtList acc cmd = LFormat.displayCmd' cmd cmdDisplay : acc
          unfinishedCmds = T.intercalate ", " $ foldl' toTxtList [] incompleteCmds

      putRegionLog region $
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

maybeSendLogToQueue ::
  ( HasFileLogging env,
    MonadIO m
  ) =>
  Log ->
  ShellT env m ()
maybeSendLogToQueue log@MkLog {dest} =
  case dest of
    LogConsole -> pure ()
    _ -> do
      fileLogging <- asks getFileLogging
      case fileLogging of
        Nothing -> pure ()
        Just (_, queue) -> do
          Queue.writeQueue queue log
{-# INLINEABLE maybeSendLogToQueue #-}

maybePollQueue :: (HasFileLogging env, MonadIO m) => ShellT env m ()
maybePollQueue = do
  fileLogging <- asks getFileLogging
  case fileLogging of
    Nothing -> pure ()
    Just (fp, queue) -> writeQueueToFile fp queue
{-# INLINEABLE maybePollQueue #-}

writeQueueToFile :: MonadIO m => FilePath -> LogTextQueue -> m void
writeQueueToFile fp queue = M.forever $ Queue.readQueue queue >>= traverse_ (logFile fp)
{-# INLINEABLE writeQueueToFile #-}

logFile :: MonadIO m => FilePath -> LogText -> m ()
logFile fp = liftIO . appendFileUtf8 fp . view #unLogText
{-# INLINEABLE logFile #-}
