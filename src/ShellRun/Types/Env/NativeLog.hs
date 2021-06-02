-- | Provides the 'NativeLog' type.
module ShellRun.Types.Env.NativeLog (NativeLog (..)) where

-- | Type for determining if we stream commands' logs.
data NativeLog
  = None
  | Stdout
  deriving (Show)
