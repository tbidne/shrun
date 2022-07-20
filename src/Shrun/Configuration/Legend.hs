-- | Provides types for the legend functionality.
--
-- @since 0.5
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
import Data.HashSet qualified as Set
import Data.Text.Lazy qualified as LazyT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTBuilder
import Shrun.Data.Command (Command (..))
import Shrun.Data.Legend (KeyVal (..), LegendMap)
import Shrun.Data.NonEmptySeq (NonEmptySeq (..))
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Errors when parsing the legend.
--
-- @since 0.5
newtype DuplicateKeyError = MkDuplicateKeyError Text
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
instance Exception DuplicateKeyError where
  displayException (MkDuplicateKeyError k) = "Legend error: found duplicate key: " <> unpack k

-- | Attempts to parse the given ['KeyVal'] into 'LegendMap'.
-- Duplicate keys are not allowed.
--
-- @since 0.1
linesToMap :: List KeyVal -> Either DuplicateKeyError LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f (MkKeyVal k v) mp = join $ liftA2 insertPair (Right (k, v)) mp
    insertPair (key, cmd) mp =
      case Map.lookup key mp of
        Just _ -> Left $ MkDuplicateKeyError key
        Nothing -> Right $ Map.insert key cmd mp

-- | @since 0.5
newtype CyclicKeyError = MkCyclicKeyError Text
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
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
-- >>> import Shrun.Data.NonEmptySeq (unsafeFromList, singleton)
-- >>> :{
--   let m = Map.fromList
--         [ ("cmd1", singleton "one"),
--           ("cmd2", singleton "two"),
--           ("all", unsafeFromList ["cmd1","cmd2","other"])
--         ]
--       cmds = translateCommands m (unsafeFromList ["all", "blah"])
--   in (fmap . fmap) (view #command) cmds
-- :}
-- Right ("one" :|^ fromList ["two","other","blah"])
--
-- Note: If -- when looking up a line -- we detect a cycle, then a 'CyclicKeyErr'
-- will be returned.
--
-- >>> :{
--   let m = Map.fromList
--         [ ("a", singleton "b"),
--           ("b", singleton "c"),
--           ("c", singleton "a")
--         ]
--   in translateCommands m (singleton "a")
-- :}
-- Left (MkCyclicKeyError "a -> b -> c -> a")
--
-- @since 0.1
translateCommands :: LegendMap -> NonEmptySeq Text -> Either CyclicKeyError (NonEmptySeq Command)
translateCommands mp (t :|^ ts) = sequenceA $ U.foldMap1 (lineToCommands mp) t ts

lineToCommands :: LegendMap -> Text -> NonEmptySeq (Either CyclicKeyError Command)
lineToCommands mp = go Nothing Set.empty (LTBuilder.fromText "")
  where
    -- The stringbuilder path is a textual representation of the key path
    -- we have traversed so far, e.g., a -> b -> c
    go prevKey foundKeys path line = case Map.lookup line mp of
      -- The line isn't a key, return it.
      Nothing -> NESeq.singleton (Right (MkCommand prevKey line))
      -- The line is a key, check for cycles and recursively
      -- call.
      Just val -> case maybeCyclicVal of
        Just cyclicVal ->
          let pathTxt = builderToPath path line cyclicVal
           in NESeq.singleton $ Left (MkCyclicKeyError pathTxt)
        Nothing -> val >>= go (Just line) foundKeys' path'
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
          neToSet = Set.fromList . NESeq.toList

builderToPath :: Builder -> Text -> Text -> Text
builderToPath path l v =
  LazyT.toStrict $
    LTBuilder.toLazyText $
      path <> LTBuilder.fromText l <> " -> " <> LTBuilder.fromText v
