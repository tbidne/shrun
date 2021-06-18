-- | Provides the 'SubLogging' type.
module ShellRun.Types.Env.SubLogging (SubLogging (..)) where

-- | Type for determining if we stream commands' logs.
data SubLogging
  = None
  | Combine
  | Native
  deriving (Show)
