{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.3
module Functional.FuncEnv
  ( FuncEnv (..),
  )
where

import Functional.Prelude
import Shrun.Configuration.Env.Types
  ( Env,
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.ShellT (ShellT)
import System.Console.Regions qualified as Regions

-- | @since 0.3
data FuncEnv = MkFuncEnv
  { coreEnv :: !Env,
    logs :: !(IORef (List Text))
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view (#coreEnv % #timeout)

-- | @since 0.3
instance HasLogging FuncEnv where
  getLogging = view (#coreEnv % #logging)

-- | @since 0.3
instance HasCompletedCmds FuncEnv where
  getCompletedCmds = view (#coreEnv % #completedCmds)

-- | @since 0.3
instance HasCommands FuncEnv where
  getCommands = view (#coreEnv % #commands)

-- | @since 0.3
instance RegionLogger (ShellT FuncEnv IO) where
  logFn txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logModeToRegionFn _ _ = logFn

  withConsoleRegion = Regions.withConsoleRegion
