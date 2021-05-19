{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Commands
  ( translateCommands,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendMap)

-- | Returns a list of 'T.Text' commands, potentially transforming a
-- given string via the `Map.Map` legend.
--
-- For a string \(s = s_1,\ldots,s_n\), we split \(s\) by commas then recursively
-- search on each \(s_i\). We stop and return \(s_i\) when it does not exist
-- as a key in the map.
--
-- For example,
--
-- @
--   m = { "cmd1": "one", "cmd2": "two", "all": "cmd1,cmd2,other" }
--   translateCommands m ["all", "blah"] == ["one", "two", "other", "blah"]
-- @
translateCommands :: LegendMap -> [Text] -> [Command]
translateCommands mp = foldMap (lineToCommands mp)

lineToCommands :: LegendMap -> Text -> [Command]
lineToCommands mp line =
  case T.splitOn "," line of
    [l] ->
      case Map.lookup l mp of
        Just c -> lineToCommands mp c
        Nothing -> [MkCommand l]
    xs -> xs >>= lineToCommands mp
