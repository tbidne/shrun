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
  case T.break (== '=') l of
    -- This is actually impossible for a non-empty string, which `l`
    -- /should/ be by this point, but including it for completeness...
    ("", _) -> Left $ ParseErr $ "Key cannot be empty: " <> l
    (_, "") -> Left $ ParseErr $ "Value cannot be empty: " <> l
    -- T.tail is safe because v can't be empty, or it would have matched
    -- the previous pattern.
    (k, v) -> Right (k, T.tail v)