-- | Provides the 'SubLogging' type.
module ShellRun.Types.Env.SubLogging (SubLogging (..)) where

-- | Type for determining if we stream commands' logs.
data SubLogging
  = -- | No logging of sub-commands
    None
  | -- | Combines sub-command logs with main process
    Combine
  | -- | Each sub-command is responsible for its own logging
    Native
  deriving (Show)
