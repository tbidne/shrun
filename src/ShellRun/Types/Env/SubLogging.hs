-- | Provides the 'SubLogging' type.
module ShellRun.Types.Env.SubLogging (SubLogging (..)) where

-- | Type for determining if we stream commands' logs.
data SubLogging
  = -- | No logging of sub-commands
    Disabled
  | -- | Logging of sub-commands
    Enabled
  deriving (Show)
