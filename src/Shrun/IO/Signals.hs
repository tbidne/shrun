{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Effectful.Concurrent (myThreadId, throwTo)
import Effectful.Posix.Signals.Static qualified as Signals
import Effectful.Process (Pid)
import Effectful.Process qualified as P
import Shrun.Configuration.Env.Types
  ( HasCommands (getCleanup),
    HasLogging,
  )
import Shrun.Data.Text qualified as Text
import Shrun.Logging qualified as Logging
import Shrun.Logging.RegionLogger (RegionLogger)
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
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    PosixSignals :> es,
    RegionLogger r :> es,
    Reader env :> es,
    Time :> es
  ) =>
  Eff es ()
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

        Logging.putRegionLogDirect @env @r baseLog

        -- Need to throw exception to main thread since this handler is run
        -- in a different thread.
        throwTo tid MkTermException

  void $ Signals.installHandler Posix.sigTERM handler Nothing

-- | Kills children for the given pid.
killChildPids ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Maybe Pid ->
  Eff es ()
killChildPids Nothing = Logging.putDebugLogDirect @env @r "killChildPids: No pid given"
killChildPids (Just pid) = do
  pidsStr <- getChildPids @env @r False (Just pid)
  pidsToKill <- filterM (canKillPid @env @r) pidsStr
  killPids @env @r pidsToKill

getChildPids ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  -- | Is multithreaded. Used for logging.
  Bool ->
  Maybe Pid ->
  Eff es (List Pid)
getChildPids _ Nothing = pure []
getChildPids multiThreads (Just pid) = do
  asks @env getCleanup >>= \case
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
        then Logging.putDebugLog @env @r
        else Logging.putDebugLogDirect @env @r

-- | Sends 'kill -15' to the list of pids.
killPids ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  List Pid ->
  Eff es ()
killPids [] = pure ()
killPids pids =
  void
    . runKill @env @r "-15"
    $ pids

canKillPid ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Pid ->
  Eff es Bool
canKillPid = runKill @env @r "-0" . (: [])

runKill ::
  forall env r es.
  ( HasCallStack,
    HasCommands env,
    HasLogging env r,
    Concurrent :> es,
    HandleWriter :> es,
    Process :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  String ->
  List Pid ->
  Eff es Bool
runKill signal pids = do
  asks @env getCleanup >>= \case
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
      Logging.putDebugLogDirect @env @r msg

      case ec of
        ExitSuccess -> pure True
        ExitFailure _ -> pure False
  where
    pidArgs = show <$> pids
    pidDispStr = unpack $ T.intercalate ", " (showt <$> pids)

readProcessTotal ::
  ( HasCallStack,
    Process :> es
  ) =>
  FilePath ->
  [String] ->
  String ->
  Eff es (ExitCode, String, String)
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
