module ShellRun.Types.Legend
  ( LegendErr (..),
    LegendMap,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

data LegendErr
  = FileErr Text
  | EntryErr Text
  | CyclicKeyErr Text
  | DuplicateKeyErr Text
  deriving (Eq, Show)

type LegendMap = Map Text Text