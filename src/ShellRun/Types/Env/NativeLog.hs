module ShellRun.Types.Env.NativeLog (NativeLog (..)) where

data NativeLog
  = None
  | Stdout
  deriving (Show)