{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
module Shrun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    CommandCleanup (..),
    updateCommandStatus,
    getReadCommandStatus,
    HasCommandLogging (..),
    HasCommonLogging (..),
    HasConsoleLogging (..),
    HasFileLogging (..),
    HasTimeout (..),
    setTimedOut,
    whenTimedOut,
    HasInit (..),
    HasAnyError (..),
    setAnyErrorTrue,
    HasNotifyConfig (..),

    -- ** Aggregate
    HasLogging,

    -- * Types
    Env (..),
    whenDebug,

    -- * Misc
    formatTimeSpec,
  )
where

import Data.HashMap.Strict qualified as Map
import Effects.Time (TimeSpec)
import Shrun.Command.Types
  ( CommandP1,
    CommandStatus,
    CommandStatusMap,
    TCommandStatusMap,
    readCommandStatus,
  )
import Shrun.Configuration.Data.CommandLogging (CommandLoggingEnv)
import Shrun.Configuration.Data.CommonLogging (CommonLoggingEnv)
import Shrun.Configuration.Data.ConfigPhase (ConfigPhase (ConfigPhaseEnv))
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat qualified as TimerFormat
import Shrun.Configuration.Data.Core (CoreConfigP)
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Configuration.Data.Notify (NotificationEnv)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Data.Text (UnlinedText)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- TODO: When we can (i.e. process provides OsPath API), these types should
-- be changed to OsPath.
data CommandCleanup = MkCommandCleanup
  { findPidsExe :: FilePath,
    killPidsExe :: FilePath
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''CommandCleanup

-- | The commands themselves.
class HasCommands env where
  -- | Retrieves the cleanup functions, if they exist.
  getCleanup :: env -> Maybe CommandCleanup

  -- | Retrieves full command graph.
  getCommandDepGraph :: env -> CommandGraph

  -- | Retrieves commands and their statuses.
  getCommandStatusMap :: env -> TCommandStatusMap

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> WithDisabled Timeout
  getHasTimedOut :: env -> TVar Bool

-- | Init, if any.
class HasInit env where
  getInit :: env -> Maybe Text

class HasCommandLogging env where
  getCommandLogging :: env -> CommandLoggingEnv

class HasCommonLogging env where
  getCommonLogging :: env -> CommonLoggingEnv

class HasConsoleLogging env r where
  getConsoleLogging ::
    env ->
    Tuple3
      -- Console logging config
      ConsoleLoggingEnv
      -- Console log region queue
      (TBQueue (LogRegion r))
      -- Console timer region
      (IORef (Maybe r))

class HasFileLogging env where
  getFileLogging :: env -> Maybe FileLoggingEnv

class HasAnyError env where
  -- | Retrieves the anyError flag.
  getAnyError :: env -> TVar Bool

-- | The main 'Env' type used by Shrun.
data Env notifyEnv logRegion = MkEnv
  { -- | Holds the anyError flag, signaling if any command exited with an
    -- error.
    anyError :: TVar Bool,
    -- | Functions to clean up running commands.
    commandCleanup :: Maybe CommandCleanup,
    -- | Holds notification environment.
    -- | Commands
    commands :: NESeq CommandP1,
    -- | Command graph.
    commandGraph :: CommandGraph,
    -- | Map from CommandIndex to Command and its status. Used for determining
    -- e.g. which commands have completed / failed / not run.
    --
    -- The statuses are TVars since they are mutable, though the map itself
    -- can be pure since its structure is fixed at initialization. In fact,
    -- we could probably swap TVar for IORef since we only update the
    -- status from a single thread (each command has its own thread).
    commandStatusMap :: TCommandStatusMap,
    -- | Core config.
    config :: CoreConfigP ConfigPhaseEnv notifyEnv,
    -- | Console log queue.
    consoleLogQueue :: ~(TBQueue (LogRegion logRegion)),
    -- Flag for if shrun has timed out, for conditionally running cleanup.
    hasTimedOut :: TVar Bool,
    -- | Timer region. It's an IORef only because it is not initialized on
    -- startup. Once it is set it is no longer mutated.
    timerRegion :: IORef (Maybe logRegion)
  }

makeFieldLabelsNoPrefix ''Env

instance HasTimeout (Env m r) where
  getTimeout = view (#config % #timeout)

  getHasTimedOut = view #hasTimedOut

instance HasInit (Env m r) where
  getInit = view (#config % #init)

instance HasCommandLogging (Env m r) where
  getCommandLogging = view (#config % #commandLogging)

instance HasCommonLogging (Env m r) where
  getCommonLogging = view (#config % #commonLogging)

instance HasConsoleLogging (Env m r) r where
  getConsoleLogging env =
    ( env ^. #config % #consoleLogging,
      env ^. #consoleLogQueue,
      env ^. #timerRegion
    )

instance HasFileLogging (Env m r) where
  getFileLogging = view (#config % #fileLogging)

instance HasCommands (Env m r) where
  getCleanup = view #commandCleanup

  getCommandDepGraph = view #commandGraph

  getCommandStatusMap = view #commandStatusMap

-- | Prepends a completed command.
updateCommandStatus ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m,
    MonadThrow m
  ) =>
  CommandP1 ->
  CommandStatus ->
  m ()
updateCommandStatus command result = do
  commandStatusMap <- asks getCommandStatusMap <&> view #unCommandStatusMap
  case Map.lookup idx commandStatusMap of
    Nothing -> throwText $ prettyToText idx
    Just (_, statusVar) -> writeTVarA' statusVar result
  where
    idx = command ^. #index
{-# INLINEABLE updateCommandStatus #-}

instance HasAnyError (Env m r) where
  getAnyError = view #anyError

-- | Set anyError to 'True'.
setAnyErrorTrue ::
  ( HasAnyError env,
    HasCallStack,
    MonadAtomic m,
    MonadReader env m
  ) =>
  m ()
setAnyErrorTrue = asks getAnyError >>= \ref -> writeTVarA' ref True
{-# INLINEABLE setAnyErrorTrue #-}

-- | Class for retrieving the notify config.
class HasNotifyConfig env r where
  -- | Retrieves the notify config.
  getNotifyConfig :: env -> Maybe (NotificationEnv r)

instance HasNotifyConfig (Env notifyEnv r) notifyEnv where
  getNotifyConfig = view (#config % #notifications)

-- | Run the action when the debug flag is active.
whenDebug :: (HasCommonLogging env, MonadReader env m) => m () -> m ()
whenDebug m = do
  debug <- asks (view (#debug % #unDebug) . getCommonLogging)
  when debug m

-- | Retrieves the entire status map in a single STM transaction.
getReadCommandStatus ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m
  ) =>
  m CommandStatusMap
getReadCommandStatus = asks getCommandStatusMap >>= readCommandStatus

-- | Sets timedout to true.
setTimedOut :: (HasTimeout env, MonadAtomic m, MonadReader env m) => m ()
setTimedOut = asks getHasTimedOut >>= \r -> writeTVarA' r True

-- | Run the action when shrun has timed out.
whenTimedOut :: (HasTimeout env, MonadAtomic m, MonadReader env m) => m () -> m ()
whenTimedOut m = do
  hasTimedOut <- readTVarA' =<< asks getHasTimedOut
  when hasTimedOut m

formatTimeSpec ::
  forall env m.
  ( HasConsoleLogging env (Region m),
    MonadReader env m
  ) =>
  TimeSpec ->
  m UnlinedText
formatTimeSpec totalTime = do
  timerFormat <- asks (view (_1 % #timerFormat) . getConsoleLogging @_ @(Region m))
  pure
    $ TimerFormat.formatRelativeTime
      timerFormat
      (Utils.timeSpecToRelTime totalTime)
{-# INLINEABLE formatTimeSpec #-}

-- | Alias for all logging config.
type HasLogging env m =
  ( HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env
  )
