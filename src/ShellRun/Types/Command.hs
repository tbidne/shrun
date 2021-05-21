module ShellRun.Types.Command (Command (..)) where

import Data.Text (Text)

newtype Command = MkCommand {getCommand :: Text}
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Text