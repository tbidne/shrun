-- | Provides types for the legend functionality.
module ShellRun.Data.Legend
  ( LegendErr (..),
    LegendMap,
  )
where

import Data.HashMap.Strict (HashMap)
import ShellRun.Prelude

-- | Various errors that can occur while processing the legend.
data LegendErr
  = -- | Errors relating to locating the legend file itself.
    FileErr Text
  | -- | Errors relating to legend key=val format.
    EntryErr Text
  | -- | Errors relating to cyclic keys.
    CyclicKeyErr Text
  | -- | Errors relating to duplicate keys.
    DuplicateKeyErr Text
  deriving (Eq, Show)

-- | Alias for our legend map.
type LegendMap = HashMap Text Text
