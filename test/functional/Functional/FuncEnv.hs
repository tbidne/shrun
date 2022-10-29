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
import Shrun.Effects.Process (Process (..))
import Shrun.IO qualified as ShIO
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.ShellT (ShellT)
import System.Console.Regions (ConsoleRegion)
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
  getDisableLogging = getDisableLogging . view #coreEnv
  getCmdLogging = getCmdLogging . view #coreEnv
  getCmdDisplay = getCmdDisplay . view #coreEnv
  getCmdLogStripControl = getCmdLogStripControl . view #coreEnv
  getCmdLogLineTrunc = getCmdLogLineTrunc . view #coreEnv
  getCmdLogNameTrunc = getCmdLogNameTrunc . view #coreEnv
  getFileLogging = getFileLogging . view #coreEnv
  getFileLogStripControl = getFileLogStripControl . view #coreEnv

-- | @since 0.3
instance HasCompletedCmds FuncEnv where
  getCompletedCmds = view (#coreEnv % #completedCmds)

-- | @since 0.3
instance HasCommands FuncEnv where
  getCommands = view (#coreEnv % #commands)

-- | @since 0.3
instance RegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ConsoleRegion

  logFn txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logModeToRegionFn _ _ = logFn

  withConsoleRegion = Regions.withConsoleRegion

-- | @since 0.5
instance Process (ShellT FuncEnv IO) where
  tryCmd = ShIO.tryCommand
  tryCmdStream = ShIO.tryCommandStreamNoRegion
  tryCmdStreamRegion = ShIO.tryCommandStreamRegion
