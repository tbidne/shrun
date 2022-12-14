{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.3
module Functional.FuncEnv
  ( FuncEnv (..),
  )
where

import Data.Sequence (Seq)
import Functional.Prelude
import Shrun.Configuration.Env.Types
  ( HasCommands (..),
    HasLogging (..),
    HasTimeout (..),
    Logging,
  )
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Timeout (Timeout)
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (..))
import Shrun.ShellT (ShellT)

{-
data Env = MkEnv
  { -- | Timeout.
    --
    -- @since 0.1
    timeout :: !(Maybe Timeout),
    -- | Logging env.
    --
    -- @since 0.7
    logging :: !(Logging ConsoleRegion),
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    --
    -- @since 0.1
    completedCmds :: !(TVar (Seq Command)),
    -- | The commands to run.
    --
    -- @since 0.1
    commands :: !(NonEmptySeq Command)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''Env
-}

-- NOTE: FuncEnv is essentially the real Env w/ an IORef for logs and a
-- simplified logging

-- | @since 0.3
data FuncEnv = MkFuncEnv
  { timeout :: !(Maybe Timeout),
    logging :: !(Logging ()),
    completedCmds :: !(TVar (Seq Command)),
    commands :: !(NonEmptySeq Command),
    logs :: !(IORef (List Text))
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view #timeout

-- | @since 0.3
instance HasLogging FuncEnv () where
  getLogging = view #logging

-- | @since 0.3
instance HasCommands FuncEnv where
  getCommands = view #commands
  getCompletedCmds = view #completedCmds

-- | @since 0.3
instance MonadRegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ()

  logGlobal txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logRegion _ _ = logGlobal

  withRegion _layout regionToShell = regionToShell ()

  displayRegions = id
