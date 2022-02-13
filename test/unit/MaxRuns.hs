-- | Provides the 'MaxRuns' type.
--
-- @since 0.1.0.0
module MaxRuns
  ( MaxRuns (..),
  )
where

import Data.Tagged (Tagged (..))
import Hedgehog (TestLimit)
import ShellRun.Prelude
import Test.Tasty.Options (IsOption (..))
import Text.Read qualified as TR

-- | Sets the maximum successful runs for each test.
--
-- @since 0.1.0.0
newtype MaxRuns = MkMaxRuns TestLimit
  deriving stock (Show)
  deriving (Num) via TestLimit

-- | @since 0.1.0.0
instance IsOption MaxRuns where
  defaultValue = MkMaxRuns 100
  parseValue = readLimit
  optionName = Tagged "max-runs"
  optionHelp = Tagged "The maximum number of runs for each test."

readLimit :: List Char -> Maybe MaxRuns
readLimit = fmap (fromIntegral @Int) . TR.readMaybe
