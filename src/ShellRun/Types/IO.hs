module ShellRun.Types.IO
  ( Stdout (..),
    Stderr (..),
  )
where

import Data.Text (Text)

newtype Stdout = MkStdout {getStdout :: Text}

newtype Stderr = MkStderr {getStderr :: Text}
