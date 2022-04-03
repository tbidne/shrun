-- | This module is the entry point to the @Shell-Run@ library used by
-- the @Shell-Run@ executable.
--
-- @since 0.1
module ShellRun
  ( ShellT,
    runShellT,
    MonadShell.runShell,
  )
where

import ShellRun.Class.MonadShell qualified as MonadShell
import ShellRun.ShellT (ShellT, runShellT)
