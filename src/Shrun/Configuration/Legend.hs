-- | Provides types for the legend functionality.
module Shrun.Configuration.Legend
  ( -- * Parsing
    linesToMap,
    LegendMap,
    DuplicateKeyError (..),

    -- * Translation
    translateCommands,
    CyclicKeyError (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Lazy qualified as LazyT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTBuilder
import Shrun.Configuration.Toml.Legend (KeyVal (MkKeyVal), LegendMap)
import Shrun.Data.Command (CommandP (MkCommandP), CommandP1)
import Shrun.Prelude

-- $setup
-- >>> import Shrun.Prelude
-- >>> import Data.HashMap.Strict qualified as Map

-- | Errors when parsing the legend.
newtype DuplicateKeyError = MkDuplicateKeyError Text
  deriving stock (Eq, Show)

instance Exception DuplicateKeyError where
  displayException (MkDuplicateKeyError k) = "Legend error: found duplicate key: " <> unpack k

-- | Attempts to parse the given ['KeyVal'] into 'LegendMap'.
-- Duplicate keys are not allowed.
linesToMap :: List KeyVal -> Either DuplicateKeyError LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f (MkKeyVal k v) mp = join $ liftA2 insertPair (Right (k, v)) mp
    insertPair (key, cmd) mp =
      case Map.lookup key mp of
        Just _ -> Left $ MkDuplicateKeyError key
        Nothing -> Right $ Map.insert key cmd mp

newtype CyclicKeyError = MkCyclicKeyError Text
  deriving stock (Eq, Show)

instance Exception CyclicKeyError where
  displayException (MkCyclicKeyError path) =
    "Encountered cyclic definitions when translating commands: " <> unpack path

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
-- ==== __Examples__
-- >>> :set -XOverloadedLists
-- >>> :{
--   let m = Map.fromList
--         [ ("cmd1", "one" :<|| []),
--           ("cmd2", "two" :<|| []),
--           ("all", "cmd1" :<|| ["cmd2","other"])
--         ]
--       cmds = translateCommands m ("all" :<|| ["blah"])
--   in (fmap . fmap) (view #command) cmds
-- :}
-- Right (fromList ("one" :| ["two","other","blah"]))
--
-- Note: If -- when looking up a line -- we detect a cycle, then a 'CyclicKeyError'
-- will be returned.
--
-- >>> :{
--   let m = Map.fromList
--         [ ("a", "b" :<|| []),
--           ("b", "c" :<|| []),
--           ("c", "a" :<|| [])
--         ]
--   in translateCommands m ("a" :<|| [])
-- :}
-- Left (MkCyclicKeyError "a -> b -> c -> a")
translateCommands :: LegendMap -> NESeq Text -> Either CyclicKeyError (NESeq CommandP1)
translateCommands mp ts = join <$> traverse (lineToCommands mp) ts

lineToCommands :: LegendMap -> Text -> Either CyclicKeyError (NESeq CommandP1)
lineToCommands mp = go Nothing Set.empty (LTBuilder.fromText "")
  where
    -- The stringbuilder path is a textual representation of the key path
    -- we have traversed so far, e.g., a -> b -> c
    go :: Maybe Text -> HashSet Text -> Builder -> Text -> Either CyclicKeyError (NESeq CommandP1)
    go prevKey foundKeys path line = case Map.lookup line mp of
      -- The line isn't a key, return it.
      Nothing -> Right $ NESeq.singleton (MkCommandP prevKey line)
      -- The line is a key, check for cycles and recursively
      -- call.
      Just val -> case maybeCyclicVal of
        Just cyclicVal ->
          let pathTxt = builderToPath path line cyclicVal
           in Left $ MkCyclicKeyError pathTxt
        Nothing -> case val of
          -- NOTE: We have to split these cases up due to handling the prevKey
          -- differently. We want to pass along the key name (i.e. line)
          -- iff we have exactly one value i.e. key = val. We do _not_ want to
          -- pass this in if we have a list i.e. key = [val1, val2, ...].
          --
          -- If we did, the command output would have:
          --   [Success][all] N seconds
          --   [Success][all] N seconds
          --   ...
          --
          -- That is, we would have multiple commands sharing the same key
          -- name, hence the output would be ambiguous. To prevent this, only
          -- pass the name in when it is guaranteed we have a unique
          -- key = val mapping.
          (x :<|| IsEmpty) -> go (Just line) foundKeys' path' x
          xs -> join <$> traverse (go Nothing foundKeys' path') xs
        where
          foundKeys' = Set.insert line foundKeys
          -- Detect if we have an intersection between previously found
          -- keys and the values we just found. If so we have found a
          -- cyclic error.
          intersect = Set.intersection foundKeys (neToSet val)
          -- If there are cycles then this should be `Just cyclicVal`
          -- (this list should have at most one since we are detecting
          -- the first cycle)
          maybeCyclicVal = headMaybe $ Set.toList intersect
          path' = path <> LTBuilder.fromText line <> " -> "
          neToSet = Set.fromList . toList

builderToPath :: Builder -> Text -> Text -> Text
builderToPath path l v =
  LazyT.toStrict
    $ LTBuilder.toLazyText
    $ path
    <> LTBuilder.fromText l
    <> " -> "
    <> LTBuilder.fromText v
