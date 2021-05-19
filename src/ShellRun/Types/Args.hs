module ShellRun.Types.Args
  ( Args (..),
  )
where

import Data.Text (Text)
import ShellRun.Types.NonNegative (NonNegative)

data Args = MkArgs
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    commands :: [Text]
  }
  deriving (Show)