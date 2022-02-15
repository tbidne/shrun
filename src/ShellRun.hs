-- | This module is the entry point to the @ShellRun@ library used by
-- the @ShellRun@ executable.
--
-- @since 0.1.0.0
module ShellRun
  ( ShellT (..),
    MonadShell.runShell,
  )
where

import ShellRun.Class.MonadShell qualified as MonadShell
import ShellRun.ShellT (ShellT (..))
