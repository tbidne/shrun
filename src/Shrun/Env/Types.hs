{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
module Shrun.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    prependCompletedCommand,
    HasLogging (..),
    HasTimeout (..),
    HasInit (..),
    HasAnyError (..),
    setAnyErrorTrue,
    HasNotifyConfig (..),

    -- * Types
    Env (..),
    Logging (..),
    CmdLogging (..),
    FileLogging (..),
    NotifyEnv (..),
  )
where

import GHC.Show (appPrec, appPrec1)
import Shrun.Data.Command (CommandP1)
import Shrun.Data.KeyHide (KeyHide)
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.Truncation (TruncRegion (TCmdLine, TCmdName), Truncation)
import Shrun.Logging.Types (FileLog, LogRegion)
import Shrun.Notify.Types
  ( NotifyAction,
    NotifyConfig (MkNotifyConfig, action, timeout),
    NotifySystemP2,
    NotifyTimeout,
  )
import Shrun.Prelude
import Text.Show (showParen, showString)

data CmdLogging = MkCmdLogging
  { stripControl :: StripControl,
    lineTrunc :: Maybe (Truncation TCmdLine)
  }
  deriving stock
    ( Eq,
      Show
    )

makeFieldLabelsNoPrefix ''CmdLogging

data FileLogging = MkFileLogging
  { stripControl :: StripControl,
    log :: ~(Tuple2 Handle (TBQueue FileLog))
  }

makeFieldLabelsNoPrefix ''FileLogging

instance Show FileLogging where
  showsPrec p fl =
    showParen (p > appPrec)
      $ showString "MkFileLogging {stripControl = "
      . showsPrec appPrec1 (fl ^. #stripControl)
      . showString ", log = <(Handle, LogTextQueue)>"
      . showString "}"

-- | Holds logging data.
data Logging r = MkLogging
  { -- | Whether to display the command (key) names or the commands
    -- themselves.
    keyHide :: KeyHide,
    -- | How often to poll commands for logs, in microseconds.
    pollInterval :: PollInterval,
    -- | The max number of command characters to display in the logs.
    cmdLogSize :: Bytes B Natural,
    -- | How to format the timer.
    timerFormat :: TimerFormat,
    -- | Truncates command names in the logs.
    cmdNameTrunc :: Maybe (Truncation TCmdName),
    -- | Whether to log commands.
    cmdLog :: Maybe CmdLogging,
    -- | Console log queue.
    consoleLog :: ~(TBQueue (LogRegion r)),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    fileLog :: Maybe FileLogging
  }

-- TODO: Laziness annotations are because unit tests currently require
-- passing in dummy values with error, so we don't want them evaluated.
-- We should come up with a different method.

makeFieldLabelsNoPrefix ''Logging

instance Show (Logging r) where
  showsPrec p env =
    showParen (p > appPrec)
      $ showString "MkEnv {keyHide = "
      . showsPrec appPrec1 (env ^. #keyHide)
      . showString ", pollInterval = "
      . showsPrec appPrec1 (env ^. #pollInterval)
      . showString ", cmdLogSize = "
      . showsPrec appPrec1 (env ^. #cmdLogSize)
      . showString ", timerFormat = "
      . showsPrec appPrec1 (env ^. #timerFormat)
      . showString ", cmdNameTrunc = "
      . showsPrec appPrec1 (env ^. #cmdNameTrunc)
      . showString ", cmdLog = "
      . showsPrec appPrec1 (env ^. #cmdLog)
      . showString ", consoleLog = <TBQueue>"
      . showString ", fileLog = "
      . showsPrec appPrec1 (env ^. #fileLog)
      . showString "}"

-- | Holds notification settings.
data NotifyEnv = MkNotifyEnv
  { -- | Notification system to use.
    system :: !NotifySystemP2,
    -- | Notification action.
    action :: NotifyAction,
    -- | Timeout to use for notifications.
    timeout :: NotifyTimeout
  }

makeFieldLabelsNoPrefix ''NotifyEnv

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> NESeq CommandP1

  getCompletedCmds :: env -> TVar (Seq CommandP1)

-- | Timeout, if any.
class HasTimeout env where
  getTimeout :: env -> Maybe Timeout

-- | Init, if any.
class HasInit env where
  getInit :: env -> Maybe Text

-- | Holds logging configuration.
class HasLogging env r where
  -- | Retrieves logging env.
  getLogging :: env -> Logging r

class HasAnyError env where
  -- | Retrieves the anyError flag.
  getAnyError :: env -> TVar Bool

-- | The main 'Env' type used by Shrun.
data Env = MkEnv
  { -- | Timeout.
    timeout :: Maybe Timeout,
    -- | Shell logic to run before each command.
    init :: Maybe Text,
    -- | Logging env.
    logging :: Logging ConsoleRegion,
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    completedCmds :: TVar (Seq CommandP1),
    -- | Holds the anyError flag, signaling if any command exited with an
    -- error.
    anyError :: TVar Bool,
    -- | Holds notification environment.
    notifyEnv :: Maybe NotifyEnv,
    -- | The commands to run.
    commands :: NESeq CommandP1
  }

makeFieldLabelsNoPrefix ''Env

instance Show Env where
  showsPrec p env =
    showParen (p > appPrec)
      $ showString "MkEnv {timeout = "
      . showsPrec appPrec1 (env ^. #timeout)
      . showString ", logging = "
      . showsPrec appPrec1 (env ^. #logging)
      . showString ", completedCmds = <TVar>"
      . showString ", anyError = <TVar>"
      . showString ", commands = "
      . showsPrec appPrec1 (env ^. #commands)
      . showString "}"

instance HasTimeout Env where
  getTimeout = view #timeout

instance HasInit Env where
  getInit = view #init

instance HasLogging Env ConsoleRegion where
  getLogging = view #logging

instance HasCommands Env where
  getCommands = view #commands
  getCompletedCmds = view #completedCmds

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
  completedCmds <- asks getCompletedCmds
  modifyTVarA' completedCmds (command :<|)

instance HasAnyError Env where
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
  getNotifyConfig :: env -> Maybe NotifyConfig

instance HasNotifyConfig Env where
  getNotifyConfig env =
    (env ^. #notifyEnv) <&> \notifyEnv ->
      MkNotifyConfig
        { action = notifyEnv ^. #action,
          timeout = notifyEnv ^. #timeout
        }
