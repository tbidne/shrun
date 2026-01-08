{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
module Shrun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    CommandCleanup (..),
    updateCommandStatus,
    getReadCommandStatus,
    readCommandStatus,
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
  )
where

import Data.HashMap.Strict qualified as Map
import Shrun.Command.Types
  ( CommandIndex,
    CommandP1,
    CommandStatus,
  )
import Shrun.Configuration.Data.CommandLogging (CommandLoggingEnv)
import Shrun.Configuration.Data.CommonLogging (CommonLoggingEnv)
import Shrun.Configuration.Data.ConfigPhase (ConfigPhase (ConfigPhaseEnv))
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.Core (CoreConfigP)
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Configuration.Data.Notify (NotifyEnv)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude

-- | Alias for all logging config.
type HasLogging env m =
  ( HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env
  )

-- TODO: When we can (i.e. process provides OsPath API), these types should
-- be changed to OsPath.
data CommandCleanup = MkCommandCleanup
  { findPidsExe :: FilePath,
    killPidsExe :: FilePath
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ FilePath, b ~ FilePath) =>
  LabelOptic "findPidsExe" k CommandCleanup CommandCleanup a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandCleanup a1 a2) ->
        fmap
          (\b -> MkCommandCleanup b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ FilePath, b ~ FilePath) =>
  LabelOptic "killPidsExe" k CommandCleanup CommandCleanup a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandCleanup a1 a2) ->
        fmap
          (\b -> MkCommandCleanup a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | The commands themselves.
class HasCommands env where
  -- | Retrieves the cleanup functions, if they exist.
  getCleanup :: env -> Maybe CommandCleanup

  -- | Retrieves full command graph.
  getCommandDepGraph :: env -> CommandGraph

  -- | Retrieves commands and their statuses.
  getCommandStatus ::
    env ->
    HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus))

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
data Env r = MkEnv
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
    completedCommands :: HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus)),
    -- | Core config.
    config :: CoreConfigP ConfigPhaseEnv,
    -- | Console log queue.
    consoleLogQueue :: ~(TBQueue (LogRegion r)),
    -- Flag for if shrun has timed out, for conditionally running cleanup.
    hasTimedOut :: TVar Bool,
    -- | Timer region. It's an IORef only because it is not initialized on
    -- startup. Once it is set it is no longer mutated.
    timerRegion :: IORef (Maybe r)
  }

instance
  ( k ~ A_Lens,
    a ~ TVar Bool,
    b ~ TVar Bool
  ) =>
  LabelOptic "anyError" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv b a2 a3 a4 a5 a6 a7 a8 a9)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Maybe CommandCleanup,
    b ~ Maybe CommandCleanup
  ) =>
  LabelOptic "commandCleanup" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 b a3 a4 a5 a6 a7 a8 a9)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NESeq CommandP1,
    b ~ NESeq CommandP1
  ) =>
  LabelOptic "commands" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 b a4 a5 a6 a7 a8 a9)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ CommandGraph,
    b ~ CommandGraph
  ) =>
  LabelOptic "commandGraph" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 b a5 a6 a7 a8 a9)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus)),
    b ~ HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus))
  ) =>
  LabelOptic "completedCommands" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 b a6 a7 a8 a9)
          (f a5)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ CoreConfigP ConfigPhaseEnv,
    b ~ CoreConfigP ConfigPhaseEnv
  ) =>
  LabelOptic "config" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 b a7 a8 a9)
          (f a6)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TBQueue (LogRegion r),
    b ~ TBQueue (LogRegion r)
  ) =>
  LabelOptic "consoleLogQueue" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 a6 b a8 a9)
          (f a7)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TVar Bool,
    b ~ TVar Bool
  ) =>
  LabelOptic "hasTimedOut" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 a6 a7 b a9)
          (f a8)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ IORef (Maybe r),
    b ~ IORef (Maybe r)
  ) =>
  LabelOptic "timerRegion" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7 a8 a9) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 a6 a7 a8 b)
          (f a9)
  {-# INLINE labelOptic #-}

instance HasTimeout (Env r) where
  getTimeout = view (#config % #timeout)

  getHasTimedOut = view #hasTimedOut

instance HasInit (Env r) where
  getInit = view (#config % #init)

instance HasCommandLogging (Env r) where
  getCommandLogging = view (#config % #commandLogging)

instance HasCommonLogging (Env r) where
  getCommonLogging = view (#config % #commonLogging)

instance HasConsoleLogging (Env r) r where
  getConsoleLogging env =
    ( env ^. #config % #consoleLogging,
      env ^. #consoleLogQueue,
      env ^. #timerRegion
    )

instance HasFileLogging (Env r) where
  getFileLogging = view (#config % #fileLogging)

instance HasCommands (Env r) where
  getCleanup = view #commandCleanup

  getCommandDepGraph = view #commandGraph

  getCommandStatus = view #completedCommands

-- | Prepends a completed command.
updateCommandStatus ::
  ( HasCallStack,
    HasCommands env,
    MonadReader env m,
    MonadSTM m,
    MonadThrow m
  ) =>
  CommandP1 ->
  CommandStatus ->
  m ()
updateCommandStatus command result = do
  completedCommands <- asks getCommandStatus
  case Map.lookup idx completedCommands of
    Nothing -> throwText $ prettyToText idx
    Just (_, statusVar) -> writeTVarA statusVar result
  where
    idx = command ^. #index
{-# INLINEABLE updateCommandStatus #-}

instance HasAnyError (Env r) where
  getAnyError = view #anyError

-- | Set anyError to 'True'.
setAnyErrorTrue ::
  ( HasAnyError env,
    HasCallStack,
    MonadReader env m,
    MonadSTM m
  ) =>
  m ()
setAnyErrorTrue = asks getAnyError >>= \ref -> writeTVarA ref True
{-# INLINEABLE setAnyErrorTrue #-}

-- | Class for retrieving the notify config.
class HasNotifyConfig env where
  -- | Retrieves the notify config.
  getNotifyConfig :: env -> Maybe NotifyEnv

instance HasNotifyConfig (Env r) where
  getNotifyConfig = view (#config % #notify)

-- | Run the action when the debug flag is active.
whenDebug :: (HasCommonLogging env, MonadReader env m) => m () -> m ()
whenDebug m = do
  debug <- asks (view (#debug % #unDebug) . getCommonLogging)
  when debug m

-- | Retrieves the entire status map in a single STM transaction.
getReadCommandStatus ::
  ( HasCallStack,
    HasCommands env,
    MonadReader env m,
    MonadSTM m
  ) =>
  m (HashMap CommandIndex (Tuple2 CommandP1 CommandStatus))
getReadCommandStatus = asks getCommandStatus >>= readCommandStatus

-- | Reads a map of TVars into a pure map via a single STM transaction.
readCommandStatus ::
  ( HasCallStack,
    MonadSTM m
  ) =>
  HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus)) ->
  m (HashMap CommandIndex (Tuple2 CommandP1 CommandStatus))
readCommandStatus commandStatusRefs =
  atomically $ for commandStatusRefs (traverse readTVar)

-- | Sets timedout to true.
setTimedOut :: (HasTimeout env, MonadReader env m, MonadSTM m) => m ()
setTimedOut = asks getHasTimedOut >>= \r -> writeTVarA r True

-- | Run the action when shrun has timed out.
whenTimedOut :: (HasTimeout env, MonadReader env m, MonadSTM m) => m () -> m ()
whenTimedOut m = do
  hasTimedOut <- readTVarA =<< asks getHasTimedOut
  when hasTimedOut m
