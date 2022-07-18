{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.3
module Functional.FuncEnv
  ( FuncEnv (..),
    mkFuncEnv,
  )
where

import Functional.Prelude
import ShellRun.Configuration.Env qualified as Env
import ShellRun.Configuration.Env.Types
  ( Env,
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import ShellRun.Effects.MonadProcRunner (MonadProcRunner (..))
import ShellRun.IO qualified as ShIO
import ShellRun.Logging.RegionLogger (RegionLogger (..))
import ShellRun.ShellT (ShellT)
import System.Console.Regions (ConsoleRegion)

-- | @since 0.3
data FuncEnv = MkFuncEnv
  { coreEnv :: !Env,
    logs :: !(IORef [Text])
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view (#coreEnv % #timeout)

-- | @since 0.3
instance HasLogging FuncEnv where
  getCmdDisplay = view (#coreEnv % #cmdDisplay)
  getCmdLineTrunc = view (#coreEnv % #lineNameTrunc)
  getCmdLogging = view (#coreEnv % #cmdLogging)
  getCmdNameTrunc = view (#coreEnv % #cmdNameTrunc)
  getFileLogging = view (#coreEnv % #fileLogging)
  getDisableLogging = view (#coreEnv % #disableLogging)
  getStripControl = view (#coreEnv % #stripControl)

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

-- | @since 0.3.0.1
instance MonadProcRunner (ShellT FuncEnv IO) where
  tryTimeProc = ShIO.tryTimeSh
  tryTimeProcStream = ShIO.tryTimeShStreamNoRegion
  tryTimeProcStreamRegion = ShIO.tryTimeShStreamRegion

-- | Makes the 'FuncEnv' from CLI args.
--
-- @since 0.3
mkFuncEnv :: IO FuncEnv
mkFuncEnv = do
  ls <- newIORef []
  env <- Env.makeEnv
  pure $
    MkFuncEnv
      { coreEnv = env,
        logs = ls
      }
