{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Legend.Internal
  ( linesToMap,
  )
where

import Control.Applicative qualified as A
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

linesToMap :: [Text] -> Either LegendErr LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = A.liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) = Map.insert key cmd

parseLine :: Text -> Either LegendErr (Text, Text)
parseLine l =
  case T.splitOn "=" l of
    ["", _] -> Left $ ParseErr l
    [_, ""] -> Left $ ParseErr l
    [key, cmd] -> Right (key, cmd)
    _ -> Left $ ParseErr l