{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
module Shrun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    updateCommandStatus,
    HasCommandLogging (..),
    HasCommonLogging (..),
    HasConsoleLogging (..),
    HasFileLogging (..),
    HasTimeout (..),
    HasInit (..),
    HasAnyError (..),
    setAnyErrorTrue,
    HasNotifyConfig (..),

    -- * Types
    Env (..),
  )
where

import Data.Map.Strict qualified as Map
import Shrun.Command.Types
  ( CommandIndex,
    CommandP1,
    CommandStatus,
    CommandStatusData (CommandStatusUnit),
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
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude

-- | The commands themselves.
class HasCommands env where
  -- | Retrieves full command graph.
  getCommandDepGraph :: env -> CommandGraph

  -- | Retrieves commands and their statuses.
  getCommandStatus ::
    env ->
    TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit)))

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> WithDisabled Timeout

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
  { config :: CoreConfigP ConfigPhaseEnv,
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    --
    -- The boolean indicates success/fail (used for command dependencies).
    completedCommands :: TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit))),
    -- | Console log queue.
    consoleLogQueue :: ~(TBQueue (LogRegion r)),
    -- | Holds the anyError flag, signaling if any command exited with an
    -- error.
    anyError :: TVar Bool,
    -- | Command graph.
    commandGraph :: CommandGraph,
    -- | Holds notification environment.
    -- | Commands
    commands :: NESeq CommandP1,
    -- | Timer region. It's an IORef only because it is not initialized on
    -- startup. Once it is set it is no longer mutated.
    timerRegion :: IORef (Maybe r)
  }

instance
  ( k ~ A_Lens,
    a ~ CoreConfigP ConfigPhaseEnv,
    b ~ CoreConfigP ConfigPhaseEnv
  ) =>
  LabelOptic "config" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv b a2 a3 a4 a5 a6 a7)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit))),
    b ~ TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit)))
  ) =>
  LabelOptic "completedCommands" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 b a3 a4 a5 a6 a7)
          (f a2)
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
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 a2 b a4 a5 a6 a7)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TVar Bool,
    b ~ TVar Bool
  ) =>
  LabelOptic "anyError" k (Env r) (Env r) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 a2 a3 b a5 a6 a7)
          (f a4)
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
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 b a6 a7)
          (f a5)
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
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 b a7)
          (f a6)
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
      $ \f (MkEnv a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkEnv a1 a2 a3 a4 a5 a6 b)
          (f a7)
  {-# INLINE labelOptic #-}

instance HasTimeout (Env r) where
  getTimeout = view (#config % #timeout)

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
  getCommandDepGraph = view #commandGraph

  getCommandStatus = view #completedCommands

-- | Prepends a completed command.
updateCommandStatus ::
  ( HasCallStack,
    HasCommands env,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  CommandStatus CommandStatusUnit ->
  m ()
updateCommandStatus command result = do
  completedCommands <- asks getCommandStatus
  modifyTVarA' completedCommands (Map.insert (command ^. #index) (command, result))
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
