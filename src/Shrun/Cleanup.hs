-- | Provides cleanup logic.
module Shrun.Cleanup
  ( teardown,
    cleanupCommands,
  )
where

import Effects.Time qualified as Time
import Shrun.Command.Types (CommandStatus (CommandRunning))
import Shrun.Configuration.Data.Notify
  ( _NotifyActionsActiveCompleteAny,
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommands,
    HasLogging,
    HasNotifyConfig (getNotifyConfig),
    formatTimeSpec,
    getReadCommandStatus,
    setAnyErrorTrue,
  )
import Shrun.IO.Signals qualified as Signals
import Shrun.Logging qualified as Logging
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger)
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel
      ( LevelKilled
      ),
    LogMode (LogModeFinish),
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Notify qualified as Notify
import Shrun.Prelude

-- | Cancels running commands and prints a final log message about going
-- down. Intended to be used when shrun has been cancelled.
teardown ::
  forall m env notifyEnv.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasLogging env m,
    HasNotifyConfig env notifyEnv,
    MonadAtomic m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadNotify m,
    MonadProcess m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadTime m,
    NotifyEnvF m ~ notifyEnv
  ) =>
  Double ->
  m ()
teardown startTime = do
  endTime <- Time.getMonotonicTime
  let totalTime = Time.fromSeconds $ endTime - startTime
  timeFormatted <- formatTimeSpec totalTime

  let cancelTasksMsg = "Received cancel"
      finalErrMsg = cancelTasksMsg <> " after running for: " <> timeFormatted

  -- update anyError
  setAnyErrorTrue

  (mWaitingLog, mRunningLog) <- Logging.mkUnfinishedCmdLogs

  -- NOTE: Manual logging because the logging queues have been shutdown at this
  -- point. We must write to the console (logRegion) and file (logFile)
  -- directly.

  -- 1. Send message about cancelling commands.
  traverse_ Logging.putRegionMultiLineLogDirect mWaitingLog
  traverse_ Logging.putRegionMultiLineLogDirect mRunningLog

  -- Clean up remaining commands.
  cleanupCommands

  let notifyBody = Notify.formatNotifyMessage finalErrMsg []

  -- 2. Send finished message.
  let finalLog =
        MkLog
          { cmd = Nothing,
            msg = Types.fromUnlined finalErrMsg,
            lvl = LevelKilled,
            mode = LogModeFinish
          }

  Logging.putRegionLogDirect finalLog

  -- 3. Send notification
  mCfg <- asks (getNotifyConfig @_ @notifyEnv)
  for_ mCfg $ \cfg -> do
    let urgency = cfg ^. #errUrgency % #unNotifyErrUrgency

    case cfg ^? (#actions % _NotifyActionsActiveCompleteAny) of
      -- If complete notifcations are on at all, send one
      Just _ -> Notify.sendNotif notifyBody "" urgency
      _ -> pure ()
{-# INLINEABLE teardown #-}

-- NOTE: [Command cleanup]
--
-- When shrun is going to terminate prematurely (e.g. killed externally or
-- a fatal exception is encountered), we want all subcommands to terminate
-- as well. We generally rely on our libraries to handle this automatically:
--
--   - async ensures an exception in the main thread is rethrown to all
--     subthreads.
--
--   - process forwards this exception to the running command.
--
-- While this is often enough, unfortunately there are some situations where
-- it is not. First, note that command running is complicated by the fact
-- that we are running through the shell, so e.g. "shrun 'some command'"
-- actually runs "/bin/sh -c 'some command'", which in turn runs
-- 'some command' in a platform-specific way.
--
-- For example, my local (linux) machine and CI OSX appear to immediately
-- terminate the /bin/sh command, and run 'some command' directly, whereas
-- CI Linux has both running.
--
-- Unfortunately, while an exception will terminate the /bin/sh command
-- on CI Linux, this exception does _not_ get proprogated to the underlying
-- 'some command'. To make matters worse, 'some command' has its parent PID
-- reassigned to PID 1, meaning we no longer have any connection to this
-- process.
--
-- To combat this, when we launch a command, we immediately store its PID
-- and any child PIDs in our command status map. Then, we attempt to kill all
-- of this upon cleanup. While this is overkill on some platforms,
-- it is necessary for CI linux (and presumably others), and does not appear
-- to be harmful. Note that this requires the following utilities:
--
--   - kill
--   - pgrep
--
-- Note that this _does not_ replace the need for commands to implement their
-- own cleanup as needed. That is, if a command spawns its own processes then
-- is that command's responsibilities to clean up these commands. Our cleanup
-- logic is only intended for handling the case where our spawned /bin/sh
-- does not forward the kill signal to its child.
cleanupCommands ::
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
  m ()
cleanupCommands = do
  -- Read all commands in a single transaction, then process. This should be
  -- safe in the sense that the command status map should not receive any
  -- updates because this is only called in two situations:
  --
  -- 1. All commands have finished.
  -- 2. Shrun main thread receives an exception.
  --
  -- In both cases, all command threads should have been killed hence no more
  -- status writes.
  --
  -- We cannot process the status in the same transaction -- in any case --
  -- because that would involve mixing IO effects in STM.
  commandsStatusMap <- getReadCommandStatus <&> view #unCommandStatusMap

  for_ commandsStatusMap $ \(_cmd, status) ->
    case status of
      CommandRunning (mPid, childPids) -> do
        -- 1. Kill the commands' children that were immediately spawned.
        -- This is the primary 'fix', as it is what happens on CI Linux,
        -- at least. This ensures we kill some_command when our /bin/sh
        -- commands do not forward the signal.
        --
        -- For platforms that end the /bin/sh immediately, this generally
        -- does nothing (which is fine, as then some_command will receive
        -- the normal terminate signal).
        Signals.killPids childPids

        -- 2. Needed for CI OSX to pass the test_script.sh test. That is,
        -- the spawned sleep commands are not cancelled. We have the log:
        --
        --   [Debug] Failed finding child pids of '13456': out: '', err: ' '
        --
        -- Where 13456 is the PPID of sleep command i.e. the PID of the script.
        -- This is correct, but we fail to find the child pids anyway.
        -- Either there is a bug in getChildPids, or the child's PPID has
        -- been reassigned by the time we run getChildPids, which seems
        -- more likely.
        Signals.killChildPids mPid

        -- 3. Needed for CI Linux to pass the test_script.sh test.
        for_ childPids killChildPids
      _ -> pure ()
  where
    killChildPids = Signals.killChildPids . Just
