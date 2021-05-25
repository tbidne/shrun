{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Commands
  ( translateCommands,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LazyT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTBuilder
import ShellRun.Types.Command (Command (..))
import ShellRun.Types.Legend (LegendErr (..), LegendMap)
import ShellRun.Utils qualified as Utils

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
--   m = { "cmd1": "one", "cmd2": "two", "all": "cmd1,,cmd2,,other" }
--   translateCommands m ["all", "blah"] == ["one", "two", "other", "blah"]
-- @
translateCommands :: LegendMap -> [Text] -> Either LegendErr [Command]
translateCommands mp = sequenceA . foldMap (lineToCommands mp)

lineToCommands :: LegendMap -> Text -> [Either LegendErr Command]
lineToCommands mp = go Set.empty (LTBuilder.fromText "")
  where
    -- The stringbuilder path is a textual representation of the key path
    -- we have traversed so far, e.g., a -> b -> c
    go foundKeys path line = case Map.lookup line mp of
      -- The line isn't a key, return it.
      Nothing -> [Right (MkCommand line)]
      -- The line is a key, check for cycles and recursively
      -- call.
      Just val -> case maybeCyclicVal of
        Just cyclicVal ->
          let pathTxt = builderToPath path line cyclicVal
           in [Left (CyclicKeyErr pathTxt)]
        Nothing -> cmds >>= go foundKeys' path'
        where
          -- WARN: Using a double comma right now as a delimiter. Hoping
          -- this isn't a thing that appears in real bash commands...
          cmds = T.splitOn ",," val
          foundKeys' = Set.insert line foundKeys

          -- Detect if we have an intersection between previously found
          -- keys and the values we just found. If so we have found a
          -- cyclic error.
          intersect = Set.intersection foundKeys (Set.fromList cmds)
          -- If there are cycles then this should be `Just cyclicVal`
          -- (this list should have at most one since we are detecting
          -- the first cycle)
          maybeCyclicVal = Utils.headMaybe $ Set.toList intersect
          path' = path <> LTBuilder.fromText line <> " -> "

builderToPath :: Builder -> Text -> Text -> Text
builderToPath path l v =
  LazyT.toStrict $
    LTBuilder.toLazyText $
      path <> LTBuilder.fromText l <> " -> " <> LTBuilder.fromText v