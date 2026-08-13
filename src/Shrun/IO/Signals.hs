-- | Provides posix signal utilities.
module Shrun.IO.Signals
  ( -- * Sending signals
    getChildPids,
    killChildPids,
    killPids,

    -- * Handling signals
    installTermHandler,
  )
where

import Control.Monad (filterM)
import Data.Text qualified as T
import Effects.Concurrent.Thread (myThreadId, throwTo)
import Effects.System.Posix.Signals qualified as Signals
import Effects.System.Process (Pid)
import Effects.System.Process qualified as P
import Shrun.Configuration.Env.Types
  ( HasCommands (getCleanup),
    HasLogging,
  )
import Shrun.Data.Text qualified as Text
import Shrun.Logging qualified as Logging
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger)
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelFatal),
    LogMode (LogModeFinish),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Prelude
import System.Posix qualified as Posix
import Text.Read qualified as TR

-- | Installs a handler for SIGTERM, so shrun can be cancelled with kill -15.
-- The signal is logged then rethrown to the main thread as TermException,
-- which ensures that cleanup is handled normally (i.e. subcommands killed).
-- By default, subthreads are __not__ killed when the RTS handles SIGTERM.
installTermHandler ::
  forall m env.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadHandleWriter m,
    MonadPosixSignals m,
    MonadRegionLogger m,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
installTermHandler = do
  tid <- myThreadId
  let handler = Signals.CatchInfo $ \si -> do
        let errMsg =
              "Received terminate signal: "
                <> Text.unsafeUnlinedText (showt (Posix.siginfoSignal si))
            baseLog =
              MkLog
                { cmd = Nothing,
                  msg = Types.fromUnlined errMsg,
                  lvl = LevelFatal,
                  mode = LogModeFinish
                }

        Logging.putRegionLogDirect baseLog

        -- Need to throw exception to main thread since this handler is run
        -- in a different thread.
        throwTo tid MkTermException

  void $ Signals.installHandler Posix.sigTERM handler Nothing

-- | Kills children for the given pid.
killChildPids ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  Maybe Pid ->
  m ()
killChildPids Nothing = Logging.putDebugLogDirect "killChildPids: No pid given"
killChildPids (Just pid) = do
  pidsStr <- getChildPids False (Just pid)
  pidsToKill <- filterM canKillPid pidsStr
  killPids pidsToKill

getChildPids ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  -- | Is multithreaded. Used for logging.
  Bool ->
  Maybe Pid ->
  m (List Pid)
getChildPids _ Nothing = pure []
getChildPids multiThreads (Just pid) = do
  asks getCleanup >>= \case
    Nothing -> pure []
    Just cleanup -> do
      (ec, stdout, stderr) <-
        readProcessTotal
          (cleanup ^. #findPidsExe)
          args
          "getChildPids"

      let (result, msg) = case ec of
            ExitFailure _ ->
              let m =
                    fromString
                      $ mconcat
                        [ "Failed finding child pids of '",
                          show pid,
                          "': out: '",
                          stdout,
                          "', err: '",
                          stderr,
                          "'"
                        ]
               in ([], m)
            ExitSuccess ->
              let pidsTxt =
                    T.lines
                      . T.strip
                      . pack
                      $ stdout
                  m =
                    fromString
                      $ mconcat
                        [ "Child pids of '",
                          show pid,
                          "': ",
                          unpack $ T.intercalate "," pidsTxt
                        ]
               in case traverse (TR.readMaybe . unpack) pidsTxt of
                    Nothing -> ([], fromString $ "Failed reading pid strings: " <> show pidsTxt)
                    Just pids -> (pids, m)
      logFn msg
      pure result
  where
    args = ["-P", show pid]

    logFn =
      -- If multiThreads is active then this function is possibly called from
      -- multiple threads i.e. the logs should be sent to the queue, as usual.
      --
      -- OTOH, this must have been called during termination when the queues
      -- are already shutdown, hence we should log directly.
      if multiThreads
        then Logging.putDebugLog
        else Logging.putDebugLogDirect

-- | Sends 'kill -15' to the list of pids.
killPids ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  List Pid ->
  m ()
killPids [] = pure ()
killPids pids =
  void
    . runKill "-15"
    $ pids

canKillPid ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  Pid ->
  m Bool
canKillPid = runKill "-0" . (: [])

runKill ::
  forall env m.
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m
  ) =>
  String ->
  List Pid ->
  m Bool
runKill signal pids = do
  asks getCleanup >>= \case
    Nothing -> pure False
    Just cleanup -> do
      (ec, stdout, stderr) <-
        readProcessTotal
          (cleanup ^. #killPidsExe)
          (signal : pidArgs)
          ("runKill " <> signal)

      let msg = case ec of
            ExitSuccess ->
              fromString
                $ mconcat
                  [ "Successfully ran kill ",
                    signal,
                    " with: ",
                    pidDispStr
                  ]
            ExitFailure _ ->
              fromString
                $ mconcat
                  [ "Kill ",
                    signal,
                    " with '",
                    pidDispStr,
                    "' failed: ",
                    "': out: '",
                    stdout,
                    "', err: '",
                    stderr,
                    "'"
                  ]
      Logging.putDebugLogDirect msg

      case ec of
        ExitSuccess -> pure True
        ExitFailure _ -> pure False
  where
    pidArgs = show <$> pids
    pidDispStr = unpack $ T.intercalate ", " (showt <$> pids)

readProcessTotal ::
  ( HasCallStack,
    MonadCatch m,
    MonadProcess m
  ) =>
  FilePath ->
  [String] ->
  String ->
  m (ExitCode, String, String)
readProcessTotal exe args str = do
  tryMySync (P.readProcessWithExitCode exe args str) >>= \case
    Left ex -> pure (ExitFailure 1, "", mkExeErr exe args $ displayException ex)
    Right r -> pure r

mkExeErr :: String -> [String] -> String -> String
mkExeErr exeStr args err =
  mconcat
    [ "Failed running command '",
      exeStr,
      "' with args '",
      unpack $ T.intercalate "," (pack <$> args),
      "': ",
      err
    ]
