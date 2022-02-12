-- | This module is the entry point to the @ShellRun@ library used by
-- the @ShellRun@ executable.
module ShellRun
  ( ShellT (..),
    MonadShell.runShell,
  )
where

import ShellRun.Class.MonadShell qualified as MonadShell
import ShellRun.Data.ShellT (ShellT (..))
