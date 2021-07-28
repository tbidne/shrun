-- | Provides functionality for translating 'Text' commands
-- via a 'LegendMap'.
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
import ShellRun.Data.Command (Command (..))
import ShellRun.Data.Legend (LegendErr (..), LegendMap)
import ShellRun.Utils qualified as Utils


-- | Returns a list of 'Text' commands, potentially transforming a
-- given string via the `LegendMap` @legend@.
--
-- Given a command string /s/, we first check if /s/ exists as a key in
-- @legend@. If it does not, we return /s/. If there is a key matching
-- /s/, i.e.,
--
-- @
-- legend = fromList [...,(s, v),...]
-- @
--
-- where \(v = v_1,,\ldots,,v_n\), then we recursively search on each
-- \(v_i\). We stop and return \(v_i\) when it does not exist as a key in the
-- map.
--
-- Example:
--
-- >>> :{
--   let m = Map.fromList
--         [ ("cmd1", "one"),
--           ("cmd2", "two"),
--           ("all", "cmd1,,cmd2,,other")
--         ]
--       cmds = translateCommands m ["all","blah"]
--   in (fmap . fmap) command cmds
-- :}
-- Right ["one","two","other","blah"]
--
-- Note: If -- when looking up a line -- we detect a cycle, then a 'CyclicKeyErr'
-- will be returned.
--
-- >>> :{
--   let m = Map.fromList
--         [ ("a", "b"),
--           ("b", "c"),
--           ("c", "a")
--         ]
--   in translateCommands m ["a"]
-- :}
-- Left (CyclicKeyErr "a -> b -> c -> a")
translateCommands :: LegendMap -> [Text] -> Either LegendErr [Command]
translateCommands mp = sequenceA . foldMap (lineToCommands mp)

lineToCommands :: LegendMap -> Text -> [Either LegendErr Command]
lineToCommands mp = go Nothing Set.empty (LTBuilder.fromText "")
  where
    -- The stringbuilder path is a textual representation of the key path
    -- we have traversed so far, e.g., a -> b -> c
    go prevKey foundKeys path line = case Map.lookup line mp of
      -- The line isn't a key, return it.
      Nothing -> [Right (MkCommand prevKey line)]
      -- The line is a key, check for cycles and recursively
      -- call.
      Just val -> case maybeCyclicVal of
        Just cyclicVal ->
          let pathTxt = builderToPath path line cyclicVal
           in [Left (CyclicKeyErr pathTxt)]
        Nothing -> cmds >>= go (Just line) foundKeys' path'
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
