module ShellRun.Types.Legend
  ( LegendErr (..),
    LegendMap,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

data LegendErr
  = FileErr Text
  | ParseErr Text
  deriving (Show)

type LegendMap = Map Text Text