-- | Provides convenience wrappers over 'Text'.
module ShellRun.Data.IO
  ( Stdout (..),
    Stderr (..),
  )
where

import ShellRun.Prelude

-- | Newtype wrapper for stdout.
newtype Stdout = MkStdout {getStdout :: Text}

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr {getStderr :: Text}
