-- | Provides the 'ShellT' monad transformer.
--
-- @since 0.1.0.0
module ShellRun.ShellT
  ( ShellT (..),
  )
where

import Control.Concurrent qualified as CC
import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad qualified as M
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.IO.Unlift qualified as UAsync
import Control.Monad.Loops qualified as Loops
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import GHC.IO.Handle (BufferMode (..), Handle)
import GHC.IO.Handle qualified as Handle
import Numeric.Algebra (ASemigroup (..))
import ShellRun.Class.MonadShell (MonadShell (..))
import ShellRun.Command (Command (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.TH qualified as TH
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env
  ( CommandLogging (..),
    Env (..),
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasFileLogging (..),
    HasTimeout (..),
  )
import ShellRun.Env qualified as Env
import ShellRun.IO (ReadHandleResult (..), Stderr (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Legend (LegendErr, LegendMap)
import ShellRun.Legend qualified as ParseLegend
import ShellRun.Logging.Log (Log (..), LogLevel (..), LogMode (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Logging.Queue (LogText (..), LogTextQueue)
import ShellRun.Logging.Queue qualified as Queue
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Clock (Clock (..))
import System.Clock qualified as C
import System.Console.Regions (ConsoleRegion, RegionLayout (..))
import System.Console.Regions qualified as Regions
import System.Exit (ExitCode (..))
import System.Posix.IO.ByteString qualified as PBS
import System.Posix.Terminal qualified as PTerm
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..))
import System.Process qualified as P
import UnliftIO qualified
import UnliftIO.Async qualified as UAsync

-- | `ShellT` is the main application type that runs shell commands.
--
-- @since 0.1.0.0
type ShellT :: Type -> (Type -> Type) -> Type -> Type
newtype ShellT e m a = MkShellT
  { -- | @since 0.1.0.0
    runShellT :: ReaderT e m a
  }
  deriving
    ( -- | @since 0.1.0.0
      Functor,
      -- | @since 0.1.0.0
      Applicative,
      -- | @since 0.1.0.0
      Monad,
      -- | @since 0.1.0.0
      MonadReader e,
      -- | @since 0.1.0.0
      MonadCatch,
      -- | @since 0.1.0.0
      MonadIO,
      -- | @since 0.1.0.0
      MonadMask,
      -- | @since 0.1.0.0
      MonadThrow,
      -- | @since 0.1.0.0
      MonadUnliftIO
    )
    via (ReaderT e m)
  deriving
    ( -- | @since 0.1.0.0
      MonadTrans
    )
    via (ReaderT e)

-- | @since 0.1.0.0
instance (HasFileLogging env, MonadIO m) => RegionLogger (ShellT env m) where
  type Region (ShellT env m) = ConsoleRegion

  putLog :: Log -> ShellT env m ()
  putLog log = do
    maybeSendLogToQueue log
    liftIO $ printLog putStrLn log

  putRegionLog :: ConsoleRegion -> Log -> ShellT env m ()
  putRegionLog region lg@MkLog {mode} = do
    let logFn = case mode of
          Set -> Regions.setConsoleRegion
          Append -> Regions.appendConsoleRegion
          Finish -> Regions.finishConsoleRegion

    maybeSendLogToQueue lg
    liftIO $ printLog (logFn region) lg

printLog :: (Text -> IO ()) -> Log -> IO ()
printLog fn = fn . Log.formatLog

-- | @since 0.1.0.0
instance
  ( MonadIO m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  MonadShell (ShellT Env m)
  where
  legendPathToMap :: FilePath -> ShellT Env m (Either LegendErr LegendMap)
  legendPathToMap = liftIO . ParseLegend.legendPathToMap

  runCommands :: NonEmptySeq Command -> ShellT Env m ()
  runCommands commands = Regions.displayConsoleRegions $
    UAsync.withAsync maybePollQueue $ \fileLogger -> do
      start <- liftIO $ C.getTime Monotonic
      let actions = UAsync.mapConcurrently_ runCommand commands
          actionsWithTimer = UAsync.race_ actions counter

      result :: Either SomeException () <- UnliftIO.withRunInIO $ \runner -> SafeEx.try $ runner actionsWithTimer

      UAsync.cancel fileLogger

      Regions.withConsoleRegion Linear $ \r -> do
        case result of
          Left ex -> do
            let errMsg =
                  T.pack $
                    "Encountered an exception. This is likely not an error in any of the "
                      <> "commands run but rather an error in ShellRun itself: "
                      <> SafeEx.displayException ex
                fatalLog = MkLog errMsg Fatal Finish
            putRegionLog r fatalLog
          Right _ -> pure ()

        end <- liftIO $ C.getTime Monotonic
        let totalTime = U.diffTime start end
            totalTimeTxt = "Finished! Total time elapsed: " <> TimeRep.formatTime totalTime
            finalLog = MkLog totalTimeTxt InfoBlue Finish

        putRegionLog r finalLog

        fileLogging <- asks getFileLogging
        case fileLogging of
          Nothing -> pure ()
          Just (fp, queue) -> Queue.flushQueue queue >>= traverse_ (logFile fp)

runCommand ::
  ( HasCommandDisplay env,
    HasCommandLogging env,
    HasFileLogging env,
    MonadIO m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  Command ->
  ShellT env m ()
runCommand cmd = do
  commandDisplay <- asks getCommandDisplay
  commandLogging <- asks getCommandLogging

  res <- case commandLogging of
    Disabled -> tryTimeSh cmd
    Enabled -> tryTimeShRegion cmd

  Regions.withConsoleRegion Linear $ \r -> do
    let lg = case res of
          Left (t, MkStderr err) ->
            let logTxt =
                  err
                    <> ". Time elapsed: "
                    <> TimeRep.formatTime t
             in MkLog logTxt Error Finish
          Right t ->
            let name = Env.displayCommand commandDisplay cmd
                logTxt =
                  "Successfully ran '"
                    <> name
                    <> "'. Time elapsed: "
                    <> TimeRep.formatTime t
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
counter = do
  -- This brief delay is so that our timer starts "last" i.e. after each individual
  -- command. This way the running timer console region is below all the commands'
  -- in the console.
  liftIO $ CC.threadDelay 100_000
  Regions.withConsoleRegion Linear $ \r -> do
    timeout <- asks getTimeout
    timer <- liftIO $ IORef.newIORef TH.zeroNN
    Loops.whileM_ (keepRunning r timer timeout) $ do
      elapsed <- liftIO $ do
        CC.threadDelay 1_000_000
        IORef.modifyIORef' timer (.+. TH.oneNN)
        IORef.readIORef timer
      logCounter r elapsed

logCounter ::
  ( RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  ConsoleRegion ->
  RNonNegative ->
  m ()
logCounter region elapsed = do
  let lg =
        MkLog
          { msg = "Running time: " <> TimeRep.formatTime elapsed,
            lvl = InfoCyan,
            mode = Set
          }
  putRegionLog region lg

keepRunning ::
  ( MonadIO m,
    RegionLogger m,
    Region m ~ ConsoleRegion
  ) =>
  ConsoleRegion ->
  IORef RNonNegative ->
  Maybe Timeout ->
  m Bool
keepRunning region timer mto = do
  elapsed <- liftIO $ IORef.readIORef timer
  if timedOut elapsed mto
    then do
      putRegionLog region $ MkLog "Timed out, cancelling remaining tasks." Warn Finish
      pure False
    else pure True

timedOut :: RNonNegative -> Maybe Timeout -> Bool
timedOut timer =
  \case
    Nothing -> False
    Just (MkTimeout t) -> timer > t

tryTimeSh ::
  ( HasCommandDisplay env,
    MonadIO m
  ) =>
  Command ->
  ShellT env m (Either (Tuple2 RNonNegative Stderr) RNonNegative)
tryTimeSh cmd = do
  commandDisplay <- asks getCommandDisplay
  liftIO $ do
    start <- C.getTime Monotonic
    res <- ShIO.tryShExitCode commandDisplay cmd Nothing
    end <- C.getTime Monotonic
    let diff = U.diffTime start end
    pure $ bimap (diff,) (const diff) res

tryTimeShRegion ::
  ( HasCommandDisplay env,
    HasFileLogging env,
    MonadIO m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  Command ->
  ShellT env m (Either (Tuple2 RNonNegative Stderr) RNonNegative)
tryTimeShRegion cmd@(MkCommand _ cmdTxt) =
  Regions.withConsoleRegion Linear $ \region -> do
    commandDisplay <- asks getCommandDisplay
    -- Create pseudo terminal here because otherwise we have trouble streaming
    -- input from child processes. Data gets buffered and trying to override the
    -- buffering strategy (i.e. handles returned by CreatePipe) does not work.
    (recvH, sendH) <- liftIO $ do
      (recvFD, sendFD) <- PTerm.openPseudoTerminal
      recvH <- PBS.fdToHandle recvFD
      sendH <- PBS.fdToHandle sendFD
      Handle.hSetBuffering recvH NoBuffering
      Handle.hSetBuffering sendH NoBuffering
      pure (recvH, sendH)

    -- We use the same pipe for std_out and std_err. The reason is that many
    -- programs will redirect stdout to stderr (e.g. echo ... >&2), and we
    -- will miss this if we don't check both. Because this "collapses" stdout
    -- and stderr to the same file descriptor, there isn't much of a reason to
    -- use two different handles.
    let pr =
          (P.shell (T.unpack cmdTxt))
            { std_out = UseHandle sendH,
              std_in = Inherit,
              std_err = UseHandle sendH,
              cwd = Nothing,
              -- We are possibly trying to read from these after the process
              -- closes (e.g. an error), so it is important they are not
              -- closed automatically!
              close_fds = False
            }

    start <- liftIO $ C.getTime Monotonic
    (exitCode, lastRead) <- UAsync.withRunInIO $ \runner ->
      P.withCreateProcess pr $ \_ _ _ ph -> runner $ streamOutput region cmd recvH ph
    end <- liftIO $ C.getTime Monotonic

    result <- case exitCode of
      ExitSuccess -> pure $ Right ()
      ExitFailure _ -> do
        -- Attempt a final read in case there is more data.
        remainingData <- liftIO $ ShIO.readHandle commandDisplay cmd recvH
        -- Take the most recent valid read of either the lastRead when running
        -- the process, or this final remainingData just attempted. The
        -- semigroup instance favors a successful read, otherwise we take the
        -- left.
        let lastData = case lastRead of
              Nothing -> remainingData
              Just r -> remainingData <> r

        pure $ Left $ ShIO.readHandleResultToStderr lastData
    liftIO $ do
      Handle.hClose sendH
      Handle.hClose recvH
    let diff = U.diffTime start end
        finalResult = bimap (diff,) (const diff) result
    pure finalResult

streamOutput ::
  ( HasCommandDisplay env,
    HasFileLogging env,
    MonadIO m
  ) =>
  ConsoleRegion ->
  Command ->
  Handle ->
  ProcessHandle ->
  ShellT env m (Tuple2 ExitCode (Maybe ReadHandleResult))
streamOutput region cmd recvH ph = do
  commandDisplay <- asks getCommandDisplay
  lastReadRef <- liftIO $ IORef.newIORef Nothing
  exitCode <- Loops.untilJust $ do
    result <- liftIO $ ShIO.readHandle commandDisplay cmd recvH
    case result of
      ReadErr _ -> do
        -- We occasionally get invalid reads here -- usually when the command
        -- exits -- likely due to a race condition. It would be nice to
        -- prevent these entirely, but for now ignore them, as it does not
        -- appear that we ever lose important messages.
        pure ()
      ReadSuccess out -> do
        liftIO $ IORef.writeIORef lastReadRef (Just (ReadSuccess out))
        let log = MkLog out SubCommand Set
        putRegionLog region log
      ReadNoData -> pure ()
    liftIO $ P.getProcessExitCode ph
  lastRead <- liftIO $ IORef.readIORef lastReadRef
  pure (exitCode, lastRead)

maybeSendLogToQueue ::
  ( HasFileLogging env,
    MonadIO m
  ) =>
  Log ->
  ShellT env m ()
maybeSendLogToQueue log = do
  fileLogging <- asks getFileLogging
  case fileLogging of
    Nothing -> pure ()
    Just (_, queue) -> do
      Queue.writeQueue queue log

maybePollQueue :: (HasFileLogging env, MonadIO m) => ShellT env m ()
maybePollQueue = do
  fileLogging <- asks getFileLogging
  case fileLogging of
    Nothing -> pure ()
    Just (fp, queue) -> writeQueueToFile fp queue

writeQueueToFile :: MonadIO m => FilePath -> LogTextQueue -> m void
writeQueueToFile fp queue = M.forever $ Queue.readQueue queue >>= traverse_ (logFile fp)

logFile :: MonadIO m => FilePath -> LogText -> m ()
logFile fp = liftIO . appendFileUtf8 fp . unLogText
