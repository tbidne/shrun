{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

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
import Data.HashSet qualified as Set
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text.Lazy qualified as LazyT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LTBuilder
import Data.Word (Word16)
import Shrun.Command.Types
  ( CommandIndex,
    CommandP (MkCommandP),
    CommandP1,
  )
import Shrun.Command.Types qualified as CT
import Shrun.Configuration.Data.Graph
  ( Edge,
    EdgeArgs (EdgeArgsList, EdgeArgsSequential),
    Edges (MkEdges),
  )
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Default (Default (def))
import Shrun.Configuration.Toml.Legend (KeyVal (MkKeyVal), LegendMap)
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
linesToMap :: (HasCallStack, MonadThrow m) => List KeyVal -> m LegendMap
linesToMap = foldr f (pure Map.empty)
  where
    f (MkKeyVal es k v) = insertPair (k, (v, es))
    insertPair (key, cmd) mMap = do
      mp <- mMap
      case Map.lookup key mp of
        Just _ -> throwM $ MkDuplicateKeyError key
        Nothing -> pure $ Map.insert key cmd mp

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
translateCommands ::
  forall m.
  ( HasCallStack,
    MonadThrow m
  ) =>
  LegendMap ->
  NESeq Text ->
  Maybe EdgeArgs ->
  m (Tuple2 (NESeq CommandP1) Edges)
translateCommands legendMap commands mEdgeArgs = do
  (legendMap', initKey) <- addCliLegend legendMap commands mEdgeArgs
  translateMap legendMap' initKey
{-# INLINEABLE translateCommands #-}

translateMap ::
  forall m.
  ( HasCallStack,
    MonadThrow m
  ) =>
  LegendMap ->
  Text ->
  m (Tuple2 (NESeq CommandP1) Edges)
translateMap mp initKey = do
  -- NOTE: [CLI and Legend Edges]
  --
  -- Previously, translateCommands took in the LegendMap and CLI commands
  -- (NESeq Text), and simply traversed 'go' over the CLI commands.
  --
  -- This became a bit awkward once the Legend keys could take edges, because
  -- this separation would mean we'd have to repair the Legend edges then
  -- repair the CLI edges. The repair logic is tricky, so duplicating this
  -- logic was not attractive.
  --
  -- We observed that this separation between CLI commands and LegendMap
  -- was arguably artificial, because we could just add the CLI command to the
  -- LegendMap under some InitKey, and then run translateCommands on InitKey.
  -- This therefore means the CLI and Toml aliases share the same logic
  -- (e.g. edge repairs, indexing), which is exactly what we want.
  --
  -- Still, this is slightly unsatisfactory as we require logic to conjure
  -- up an unused InitKey, which can technically fail (though a failure
  -- would be truly pathological). There are some alternatives that do not
  -- require adding the CLI commands + edges to the LegendMap:
  --
  -- 1. Treat them totally separately here. As mentioned, this means we
  --    have to duplicate the indexing + edge repair logic. Not ideal.
  --
  -- 2. Abstract the lookup function. That is, instead of doing Map.lookup
  --    in 'go', pass it in as a function. The very first lookup will
  --    hardcode returning the CLI commands + edges, the rest will use
  --    Map.lookup. This requires adding some kind of flag to the accumulator
  --    e.g. a boolean that determines which lookup to use.
  let commands = indexSeq $ NESeq.singleton initKey
  (cmds, edges, _) <- go Nothing Set.empty (LTBuilder.fromText "") one commands
  pure (cmds, Graph.sortEdges edges)
  where
    go ::
      -- Previous key, for handling the key on the Command.
      Maybe Text ->
      -- Keys found so far, for detecting cycles.
      HashSet Text ->
      -- The stringbuilder path is a textual representation of the key path
      -- we have traversed so far, e.g., a -> b -> c
      Builder ->
      -- Starting index for the next command.
      CommandIndex ->
      -- List to process: tuple of (command or alias) text, along with
      -- _original_ index, for repairing any edges that reference this text,
      -- after alias expansion changes the indexes.
      NESeq (Tuple2 CommandIndex Text) ->
      -- Accumulator of (NESeq CommandP1, Edges, Map Idx (Idx, Idx)). We
      -- accumulate commands and edges as we encounter them. We also build up
      -- a map that relates original index to new index bounds, for repairing
      -- edges.
      m Acc
    go prevKey foundKeys path startIdx ((origIdx, line) :<|| lines) = do
      case Map.lookup line mp of
        Nothing -> do
          -- The line isn't a key. Make a singleton command and continue with the rest.
          let cmds = NESeq.singleton (MkCommandP startIdx prevKey line)
              -- The new index is just the single startIdx.
              allData = (cmds, def, Map.singleton origIdx (startIdx, startIdx))
          case lines of
            Empty -> pure allData
            l :<| ls -> (allData <>) <$> go prevKey foundKeys path (CT.succ startIdx) (l :<|| ls)
        -- The line is a key, check for cycles and recursively call.
        Just (vals, mEdges) -> case maybeCyclicVal of
          Just cyclicVal -> do
            let pathTxt = builderToPath path line cyclicVal
            throwM $ MkCyclicKeyError pathTxt
          Nothing -> do
            -- 1. Run on newly found commands.
            --
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
            --
            -- We also must guard against the initKey, since we do not want
            -- that to be considered a key.
            let mPrevKey =
                  if length vals > 1 || line == initKey
                    then Nothing
                    else Just line
                -- Add indexes to the found values. These are the _original_
                -- indexes i.e. what mEdges references.
                valsIx = indexSeq vals

            -- Run 'go' on the found vals, collecting the expanded commands
            -- (subCmds), all edges (subEdges), and map from subCommands. We
            -- use this map to repair mEdges.
            (subCmds, subEdges, subCmdIdxMap) <- go mPrevKey foundKeys' path' startIdx valsIx
            let -- numCmds, subtracting one as this represents the _final_
                -- index, which is one less than the total number of commands.
                -- E.g. endIdx === startIdx <=> len(vals) === 1, hence
                -- numCmdsIdx === 0.
                numCmdsIdx = unsafeNonNegative $ length subCmds - 1
                endIdx = CT.addNN startIdx numCmdsIdx

                -- idxMap is the index map for _this_ command i.e. the line
                -- with subcommands. Its value is its original index
                -- to its new start (startIdx) and end (endIdx, which is
                -- just the length of the expanded commands). E.g., if we had
                --
                --   line => [cmd1, cmd2, cmd3]
                --
                -- Then the new indexes would be (startIdx, startIdx + 2)
                --
                -- Note that we do not want to return subCmdIdxMap as it is
                -- only relevant for repairing mEdges. The only map we should
                -- return here is idxMap.
                idxMap = Map.singleton origIdx (startIdx, endIdx)

            -- Repair the edges.
            repairedEdges <- case mEdges of
              Nothing -> pure def
              Just EdgeArgsSequential ->
                -- If our graph is sequential, make an edge list (sequential
                -- edges for original values), then repait it.
                repairEdges (mkSequentialEdges vals) subCmdIdxMap
              Just (EdgeArgsList es) -> repairEdges es subCmdIdxMap

            let newEdges = repairedEdges <> subEdges
                allData = (subCmds, newEdges, idxMap)

            -- 2. Run 'go' on the rest.
            case lines of
              Empty -> pure allData
              l :<| ls -> do
                let newIdx = startIdx .+. CT.unsafeFromInt (length subCmds)
                (allData <>) <$> go prevKey foundKeys path newIdx (l :<|| ls)
          where
            foundKeys' = Set.insert line foundKeys
            -- Detect if we have an intersection between previously found
            -- keys and the values we just found. If so we have found a
            -- cyclic error.
            intersect = Set.intersection foundKeys (neToSet vals)
            -- If there are cycles then this should be `Just cyclicVal`
            -- (this list should have at most one since we are detecting
            -- the first cycle)
            maybeCyclicVal = headMaybe $ Set.toList intersect
            path' =
              if line == initKey
                then ""
                else path <> LTBuilder.fromText line <> " -> "
            neToSet = Set.fromList . toList
{-# INLINEABLE translateMap #-}

-- | Adds indexes to the NESeq.
indexSeq :: NESeq a -> NESeq (Tuple2 CommandIndex a)
indexSeq xs = NESeq.zip (CT.unsafeFromInt <$> unsafeListToNESeq [1 .. length xs]) xs

-- | Repairs the paramter @edges@, based on the param @indexMap@. The
-- fundamental problems is that some edge @src -> dest@ may no longer be
-- correct after alias expansion. That is, suppose we have:
--
-- @
--   commands: cmd1 some_aliases cmd2
--   edges: 1 -> 2, 2 -> 3
-- @
--
-- i.e. our edges means @cmd1 -> some_aliases, some_aliases -> cmd2@
--
-- Say @some_aliases@ expands to @a1 a2 a2@. If we did nothing, our edges
-- would now mean @cmd1 -> a1, a1 -> a2@, which is not what we wanted.
-- What we really want is:
--
-- @
--   # original 'cmd1 -> some_aliases' edge
--   cmd1 -> a1, cmd1 -> a3, cmd1 -> a3
--   # original 'some_aliases -> cmd2' edge
--   a1 -> cmd2, a2 -> cmd2, a3 -> cmd2
-- @
--
-- i.e. we need to update the indexes, and when an alias is part of an edge,
-- we need to add edges for each alias index. To do this, the param
-- @indexMap@ stores a mapping from prevIndex to newIndexRange. In this case,
-- we'd have:
--
-- @
--   1 -> (1,1)
--   2 -> (2,4)
--   3 -> (5,5)
-- @
--
-- Then for each edge @src -> dest@, we look up both in the map to produce:
--
-- @
--   - Sources: (srcStart, srcEnd)
--   - Dests: (destStart, destEnd)
-- @
--
-- And make edges for the cartesian product of @Sources x Dests@ i.e. all
-- @si -> dj@.
repairEdges ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  Edges ->
  HashMap CommandIndex Edge ->
  m Edges
repairEdges (MkEdges es) idxMap = MkEdges <$> foldr mapEdge (pure Empty) es
  where
    mapEdge (src, dest) mAcc = do
      (srcStart, srcEnd) <- lookupEdge src
      (destStart, destEnd) <- lookupEdge dest

      let newEdges :: Seq Edge
          newEdges =
            [ (s, d)
            | s <- [srcStart .. srcEnd],
              d <- [destStart .. destEnd]
            ]
      (newEdges <>) <$> mAcc
      where
        lookupEdge i = case Map.lookup i idxMap of
          Nothing ->
            throwText
              $ mconcat
                [ "Index '",
                  showt (i ^. #unCommandIndex % #unPositive),
                  "' in edge '",
                  showt (src ^. #unCommandIndex % #unPositive),
                  " -> ",
                  showt (dest ^. #unCommandIndex % #unPositive),
                  "' is out-of-bounds."
                ]
          Just (s, e) -> pure (s, e)
{-# INLINEABLE repairEdges #-}

mkSequentialEdges :: NESeq Text -> Edges
mkSequentialEdges =
  MkEdges
    . dropLast
    . fmap toEdge
    . NESeq.toSeq
    . indexSeq
  where
    toEdge (idx, _) = (idx, CT.succ idx)

    dropLast Empty = Empty
    dropLast (_ :<| Empty) = Empty
    dropLast (x :<| ys) = x :<| dropLast ys

-- | Acc is our basic accumulator. In addition to the commands and edges that
-- we want to accumulate, we also have a map that relates a names old index
-- to its new index range. This is used to repair edges. For instance:
-- we have commands:
--
--   shrun --edges="1 -> 3" cmd1 some_aliases cmd2
--
-- where some_aliases expands to a1 and a2. The intention is that cmd2 needs
-- to wait for cmd1. But after alias expansion, our commands will be indexed:
--
--   cmd1 a1 a2 cmd2
--
-- And the edge will mistakenly be "cmd1 -> a2". We use the map to update
-- the edges i.e. transform that edge to "1 -> 4".
type Acc =
  Tuple3
    (NESeq CommandP1)
    Edges
    (HashMap CommandIndex (CommandIndex, CommandIndex))

builderToPath :: Builder -> Text -> Text -> Text
builderToPath path l v =
  LazyT.toStrict
    $ LTBuilder.toLazyText
    $ path
    <> LTBuilder.fromText l
    <> " -> "
    <> LTBuilder.fromText v

-- | Adds the commands and edges to the map under some unmapped key, that
-- is returned.
--
-- See NOTE: [CLI and Legend Edges]
--
-- We add the CLI commands and possible CLI edges to the legendMap e.g.
-- 'shrun_init_key_1 -> (CLI commands, CLI edges)'. We do this so
-- translateCommands can act uniformly over the map, which makes handling
-- edges correctly easier.
addCliLegend ::
  (HasCallStack, MonadThrow m) =>
  LegendMap ->
  NESeq Text ->
  Maybe EdgeArgs ->
  m (Tuple2 LegendMap Text)
addCliLegend legendMap commands mCliEdgeArgs = do
  unmappedKey <- findUnmappedKey legendMap commands
  pure (Map.insert unmappedKey (commands, mCliEdgeArgs) legendMap, unmappedKey)
{-# INLINEABLE addCliLegend #-}

-- | Finds a key that does not exist in the map or as a command name
-- (The latter is to avoid cycles).
findUnmappedKey ::
  forall m.
  (HasCallStack, MonadThrow m) =>
  LegendMap ->
  NESeq Text ->
  m Text
findUnmappedKey legendMap commands = go 0
  where
    commandSet = Set.fromList (toList commands)
    mx = maxBound
    pfx = "shrun_init_key_"

    unmapped t =
      not (Map.member t legendMap || Set.member t commandSet)

    go :: Word16 -> m Text
    go i
      | i == mx =
          throwText
            $ mconcat
              [ "Found too many ",
                pfx,
                "<i> keys in legend. Expected at least one free in range (0, ",
                showt mx,
                ")."
              ]
      | unmapped key = pure key
      | otherwise = go (i + 1)
      where
        key = pfx <> showt i
{-# INLINEABLE findUnmappedKey #-}
