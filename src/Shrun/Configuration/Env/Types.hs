{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
module Shrun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    prependCompletedCommand,
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

import Shrun.Configuration.Data.CommandLogging (CommandLoggingEnv)
import Shrun.Configuration.Data.CommonLogging (CommonLoggingEnv)
import Shrun.Configuration.Data.ConfigPhase (ConfigPhase (ConfigPhaseEnv))
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.Core (CoreConfigP)
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Data.Notify (NotifyEnv)
import Shrun.Data.Command (CommandP1)
import Shrun.Logging.Types (LogRegion)
import Shrun.Prelude

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> NESeq CommandP1

  getCompletedCommands :: env -> TVar (Seq CommandP1)

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe Timeout

-- | Init, if any.
class HasInit env where
  getInit :: env -> Maybe Text

class HasCommandLogging env where
  getCommandLogging :: env -> CommandLoggingEnv

class HasCommonLogging env where
  getCommonLogging :: env -> CommonLoggingEnv

class HasConsoleLogging env r where
  getConsoleLogging :: env -> Tuple2 ConsoleLoggingEnv (TBQueue (LogRegion r))

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
    completedCommands :: TVar (Seq CommandP1),
    -- | Console log queue.
    consoleLogQueue :: ~(TBQueue (LogRegion r)),
    -- | Holds the anyError flag, signaling if any command exited with an
    -- error.
    anyError :: TVar Bool,
    -- | Holds notification environment.
    -- | Commands
    commands :: NESeq CommandP1
  }

makeFieldLabelsNoPrefix ''Env

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
      env ^. #consoleLogQueue
    )

instance HasFileLogging (Env r) where
  getFileLogging = view (#config % #fileLogging)

instance HasCommands (Env r) where
  getCommands = view #commands
  getCompletedCommands = view #completedCommands

-- | Prepends a completed command.
prependCompletedCommand ::
  ( HasCallStack,
    HasCommands env,
    MonadReader env m,
    MonadSTM m
  ) =>
  CommandP1 ->
  m ()
prependCompletedCommand command = do
  completedCommands <- asks getCompletedCommands
  modifyTVarA' completedCommands (command :<|)

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

-- | Class for retrieving the notify config.
class HasNotifyConfig env where
  -- | Retrieves the notify config.
  getNotifyConfig :: env -> Maybe NotifyEnv

instance HasNotifyConfig (Env r) where
  getNotifyConfig = view (#config % #notify)
