{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Graph
  ( -- * Args
    EdgeArgs (..),
    Edges (..),
    sortEdges,
    Edge,
    EdgeLabel (..),

    -- * Graph
    CommandGraph (..),
    Vertex,
    LVertex,

    -- ** Creation
    mkGraph,
    mkEdgelessGraph,

    -- ** Functions
    labVertices,
    labVertex,
    labInVertices,
    outVertices,
    vertices,

    -- *** Context
    context,
    ctxLabVertex,
    ctxOutVertices,
  )
where

import Data.Graph.Inductive.Graph (Context, Node)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.Dominators qualified as Dom
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Exts (IsList (Item))
import GHC.Exts qualified as Exts
import Shrun.Command.Types
  ( CommandIndex,
    CommandOrd (MkCommandOrd),
    CommandP (MkCommandP),
    CommandP1,
    LVertex,
    Vertex,
  )
import Shrun.Command.Types qualified as Command.Types
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- Note that these 'Edge' types all refer to types that are user-facing i.e.
-- parsed. The internal graph operates an pure Ints intead of our CommandIndex.

-------------------------------------------------------------------------------
--                                User Edges                                 --
-------------------------------------------------------------------------------

-- NOTE: [User Edges]
--
-- These edge types are used at the user-config level i.e. CLI args or
-- toml configuration. Once we parse the edges and commands into a
-- 'CommandGraph', we use more appropriate vertex/edge types.

-- | Types of edges.
data EdgeLabel
  = -- | cmd1 &-> cmd2 runs cmd2 iff cmd1 succeeds.
    EdgeAnd
  | -- | cmd1 |-> cmd2 runs cmd2 iff cmd1 fails.
    EdgeOr
  | -- | cmd1 ;-> cmd2 runs cmd2 iff cmd1 finishes with any status.
    EdgeAny
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | CLI command graph. The default instance is an "edgeless graph", in the
-- sense that all commands are root nodes without any edges, hence normal
-- behavior.
data EdgeArgs
  = -- | Sequential i.e. a linear graph of success edges.
    EdgeArgsSequential
  | -- | Explicit edges.
    EdgeArgsList Edges
  deriving stock (Eq, Show)

instance Default EdgeArgs where
  def = EdgeArgsList mempty

instance IsList EdgeArgs where
  type Item EdgeArgs = Edge

  fromList = EdgeArgsList . Exts.fromList

  toList (EdgeArgsList xs) = Exts.toList xs
  toList EdgeArgsSequential = error "Called toList on EdgeArgsSequential"

-- | An edge between two indices.
type Edge = Tuple3 CommandIndex CommandIndex EdgeLabel

edgeToFgl :: Edge -> GEdge
edgeToFgl (s, d, l) =
  ( toV s,
    toV d,
    l
  )

-- | FGL edge.
type GEdge = Tuple3 Node Node EdgeLabel

-- | Dependency edges are supplied by the user on the CLI.
newtype Edges = MkEdges {unEdges :: Seq Edge}
  deriving newtype (IsList, Monoid, Semigroup)
  deriving stock (Eq, Show)

sortEdges :: Edges -> Edges
sortEdges =
  MkEdges
    . Seq.sort
    . view #unEdges

instance
  ( k ~ An_Iso,
    a ~ Seq Edge,
    b ~ Seq Edge
  ) =>
  LabelOptic "unEdges" k Edges Edges a b
  where
  labelOptic = iso (\(MkEdges es) -> es) MkEdges
  {-# INLINE labelOptic #-}

-------------------------------------------------------------------------------
--                              Command Graph                                --
-------------------------------------------------------------------------------

-- NOTE: [Command Graph]
--
-- 'CommandGraph' is merely fgl's graph type, along with the root vertexes.
-- We want other modules to use the API here rather than depend on fgl's API,
-- hence other modules should use functions/types defined here only.
--
-- Currently, our vertex types are aliases:
--
--   - type Vertex = Int
--   - type LVertex a = Tuple2 Vertex a
--
-- Which happens to match fgl's Node and LNode, respectively. We do this for
-- convenience, though a newtype would be another option.
--
-- We currently do not have any particular type for edges, as our 'edge'
-- functions return source or dest vertices directly.

-- | Command dependency graph. Morally, @Vertex == CommandIndex@.
data CommandGraph = MkCommandGraph
  { -- | Underlying graph.
    graph :: Gr CommandP1 EdgeLabel,
    -- | Root commands i.e. have no dependencies.
    roots :: NESeq Vertex
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ Gr CommandP1 EdgeLabel,
    b ~ Gr CommandP1 EdgeLabel
  ) =>
  LabelOptic "graph" k CommandGraph CommandGraph a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandGraph a1 a2) ->
        fmap
          (\b -> MkCommandGraph b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NESeq Vertex,
    b ~ NESeq Vertex
  ) =>
  LabelOptic "roots" k CommandGraph CommandGraph a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandGraph a1 a2) ->
        fmap
          (\b -> MkCommandGraph a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | Creates a command dependency graph from list of dependencies and typed
-- commands.
mkGraph ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  EdgeArgs ->
  NESeq CommandP1 ->
  m CommandGraph
mkGraph cdgArgs cmds = do
  -- Verify edges are unique
  verifyUniqueEdges edges

  -- Verify all edges exist.
  allEdgesExist edges cmdSet

  -- Find roots.
  let rootMap = Map.withoutKeys cmdMap nonRoots

  (roots, vs) <- case Map.toList rootMap of
    [] -> throwText "No root command(s) found! There is probably a cycle."
    (r : rs) -> do
      pure
        ( fmap (view _1) (r :<|| Seq.fromList rs),
          Map.toList cmdMap
        )

  let graph = G.mkGraph @Gr vs (toList edges)
      cdg =
        MkCommandGraph
          { graph,
            roots
          }

  -- Verify all nodes reachable.
  verifyAllReachable cdg cmdSet

  -- Verify no cycles. Note that it is probably impossible to have
  -- no root nodes or unreachable nodes without a cycle, so in some sense
  -- this check subsumes them. The primary caveat is that our cycle detection
  -- involves traversing from the roots, so:
  --
  --   - If we have no roots, then we cannot find any cycles.
  --   - If some nodes are unreachable, then traversing the roots will not
  --     find them, hence not find the cycle.
  --
  -- Therefore all of these checks need to be here. The alternative would be
  -- to check for cycles for every single vertex.
  verifyNoCycles cdg

  pure cdg
  where
    edges = case cdgArgs of
      EdgeArgsList es -> edgeToFgl <$> (es ^. #unEdges)
      EdgeArgsSequential -> mkSequentialEdges cmds

    -- nonRoots is all vertices with an in-edge.
    nonRoots :: Set Int

    (_, nonRoots) = foldl' mkEdgeMap (HMap.empty, Set.empty) edges

    mkEdgeMap :: EdgeAcc -> GEdge -> EdgeAcc
    mkEdgeMap (mp, nr) (s, d, _) = case HMap.lookup s mp of
      Nothing -> (HMap.insert s [d] mp, Set.insert d nr)
      Just es -> (HMap.insert s (d : es) mp, Set.insert d nr)

    cmdSet = Map.keysSet cmdMap
    cmdMap =
      Map.fromList ((\cmd -> (toV $ cmd ^. #index, cmd)) <$> toList cmds)

-- | Creates a trivial command dep graph where each command is a
-- disconnected vertex, hence root. This exists to avoid the monad in
-- 'mkGraph', as some uses want purity (defaultConfig...).
-- We /should/ have @mkGraph mempty == mkEdgelessGraph@.
mkEdgelessGraph :: NESeq CommandP1 -> CommandGraph
mkEdgelessGraph cmds =
  MkCommandGraph
    { graph,
      roots
    }
  where
    graph = G.mkGraph idxCmds []

    idxCmds = zip [1 ..] (toList cmds)

    roots = toV . view #index <$> cmds

-- | Retrieves all labeled vertices.
labVertices :: CommandGraph -> List (LVertex CommandP1)
labVertices = G.labNodes . view #graph

-- | Finds a labeled vertex. Can fail.
labVertex :: (HasCallStack, MonadEvaluate m) => CommandGraph -> Vertex -> m (LVertex CommandP1)
labVertex cg = fmap G.labNode' . context cg

-- | Retrieves all vertices.
vertices :: CommandGraph -> List Vertex
vertices = G.nodes . view #graph

-- | Given a vertex d, returns all (s, l) s.t. there exists an l-edge
-- s -> d.
labInVertices :: CommandGraph -> Vertex -> List (Tuple2 Vertex EdgeLabel)
labInVertices = G.lpre . view #graph

-- | Given a vertex s, returns all d s.t. there exists an edge s -> d.
outVertices :: CommandGraph -> Vertex -> List Vertex
outVertices cg = G.suc (cg ^. #graph)

-- | 'labVertex' that operates on the context.
ctxLabVertex :: Context a b -> LVertex a
ctxLabVertex = G.labNode'

-- | 'outVertices' that operates on the context.
ctxOutVertices :: Context a b -> List Vertex
ctxOutVertices = G.suc'

-- | Given a vertex, retrieves its context.
context ::
  ( HasCallStack,
    MonadEvaluate m
  ) =>
  CommandGraph ->
  Vertex ->
  m (Context CommandP1 EdgeLabel)
context cg = evaluate . force . G.context (cg ^. #graph)

displayCommandIndex :: CommandIndex -> Text
displayCommandIndex = showt . view (#unCommandIndex % #unPositive)

displayVertex :: Node -> Text
displayVertex = displayCommandIndex . Command.Types.fromVertex

toV :: CommandIndex -> Node
toV = Command.Types.toVertex

-- ((out) Edge Map, Non-roots)
type EdgeAcc =
  Tuple2
    (HashMap Int (List Int))
    (Set Int)

verifyUniqueEdges :: (HasCallStack, MonadThrow m) => Seq GEdge -> m ()
verifyUniqueEdges edges = case toList duplicates of
  [] -> pure ()
  es@(_ : _) -> do
    let msg = "Found duplicates: " <> T.intercalate " " (renderEdge <$> es)
    throwText msg
  where
    (_, duplicates) = foldl' go (Set.empty, Set.empty) edges

    go :: Tuple2 (Set GEdge) (Set GEdge) -> GEdge -> Tuple2 (Set GEdge) (Set GEdge)
    go (found, dupes) edge
      | Set.member edge found = (found, Set.insert edge dupes)
      | otherwise = (Set.insert edge found, dupes)

    renderEdge (s, t, _) =
      mconcat
        [ "(",
          showt s,
          ",",
          showt t,
          ")"
        ]

-- | Verifies all commands references in the edges exist.
allEdgesExist :: (HasCallStack, MonadThrow m) => Seq GEdge -> Set Int -> m ()
allEdgesExist edges cmds = for_ edges $ \(s, d, _) -> do
  if
    | s `notMember` cmds ->
        throwText $ mkErr s (s, d)
    | d `notMember` cmds ->
        throwText $ mkErr d (s, d)
    | otherwise -> pure ()
  where
    notMember x = not . Set.member x

    mkErr x (s, d) =
      mconcat
        [ "Command index ",
          showt x,
          " in dependency ",
          showt s,
          " -> ",
          showt d,
          " does not exist."
        ]

verifyAllReachable :: (HasCallStack, MonadThrow m) => CommandGraph -> Set Int -> m ()
verifyAllReachable cdg cmdSet = do
  case toList nonReachable of
    [] -> pure ()
    xs@(_ : _) -> do
      let msg =
            mconcat
              [ "The following commands are not reachable: ",
                T.intercalate ", " (showt <$> xs),
                ". There is probably a cycle."
              ]
      throwText msg
  where
    nonReachable = Set.difference cmdSet reachable
    reachable = reachableFromRoots cdg

-- Dominators seems to do what we want, but if not we can write this manually.
reachableFromRoots :: CommandGraph -> Set Node
reachableFromRoots cdg =
  Set.fromList
    . fmap (view _1)
    . (Dom.dom graph <=< toList)
    $ cdg
    ^. #roots
  where
    graph = cdg ^. #graph

-- | Verifies that no cycles exist.
verifyNoCycles :: (HasCallStack, MonadThrow m) => CommandGraph -> m ()
verifyNoCycles cdg = traverse_ (traverseVertex cdg) (cdg ^. #roots)

traverseVertex :: forall m. (HasCallStack, MonadThrow m) => CommandGraph -> Node -> m ()
traverseVertex cdg = go (HSet.empty, [])
  where
    fromV :: Node -> List Node
    fromV = G.suc (cdg ^. #graph)

    go :: CycleAcc -> Node -> m ()
    go (foundVs, path) v = do
      if HSet.member v foundVs
        then do
          let msg =
                mconcat
                  [ "Found command cycle: ",
                    renderPath (L.reverse $ v : path)
                  ]
          throwText msg
        else do
          let es = fromV v
          traverse_ (go (HSet.insert v foundVs, v : path)) es

    renderPath = T.intercalate " -> " . fmap displayVertex

type CycleAcc = (HashSet Node, [Node])

mkSequentialEdges :: NESeq CommandP1 -> Seq GEdge
mkSequentialEdges =
  dropLast
    . fmap toEdge
    . Seq.sortOn MkCommandOrd
    . NESeq.toSeq
  where
    toEdge (MkCommandP idx _ _) =
      ( toV idx,
        toV $ Command.Types.succ idx,
        EdgeAnd
      )

    dropLast Empty = Empty
    dropLast (_ :<| Empty) = Empty
    dropLast (x :<| ys) = x :<| dropLast ys
