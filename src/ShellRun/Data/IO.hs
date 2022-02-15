-- | Provides convenience wrappers over 'Text'.
--
-- @since 0.1.0.0
module ShellRun.Data.IO
  ( Stdout (..),
    Stderr (..),
  )
where

import ShellRun.Prelude

-- | Newtype wrapper for stdout.
--
-- @since 0.1.0.0
newtype Stdout = MkStdout
  { -- | @since 0.1.0.0
    getStdout :: Text
  }

-- | Newtype wrapper for stderr.
--
-- @since 0.1.0.0
newtype Stderr = MkStderr
  { -- | @since 0.1.0.0
    getStderr :: Text
  }
