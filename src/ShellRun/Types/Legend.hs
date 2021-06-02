-- | Provides types for the legend functionality.
module ShellRun.Types.Legend
  ( LegendErr (..),
    LegendMap,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | Various errors that can occur while processing the legend.
data LegendErr
  = FileErr Text
  -- ^Errors relating to locating the legend file itself.
  | EntryErr Text
  -- ^Errors relating to legend key=val format.
  | CyclicKeyErr Text
  -- ^Errors relating to cyclic keys.
  | DuplicateKeyErr Text
  -- ^Errors relating to duplicate keys.
  deriving (Eq, Show)

-- | Alias for our legend map.
type LegendMap = Map Text Text
