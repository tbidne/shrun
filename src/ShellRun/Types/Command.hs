{-# LANGUAGE DerivingVia #-}

-- | Provides the 'Command' wrapper for commands.
module ShellRun.Types.Command (Command (..)) where

import Data.Text (Text)

-- | Newtype wrapper for shell commands.
newtype Command = MkCommand {getCommand :: Text}
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Text
