{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Graph
  ( -- * Args
    EdgeArgs (..),
    Edges (..),
    sortEdges,
    Edge,
    EdgeSequential (..),
    EdgeLabel (..),
    displayEdgeLabel,

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
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
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
  = -- | cmd1 & cmd2 runs cmd2 iff cmd1 succeeds.
    EdgeAnd
  | -- | cmd1 | cmd2 runs cmd2 iff cmd1 fails.
    EdgeOr
  | -- | cmd1 ; cmd2 runs cmd2 iff cmd1 finishes with any status.
    EdgeAny
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

instance Pretty EdgeLabel where
  pretty = displayEdgeLabel

displayEdgeLabel :: (IsString s) => EdgeLabel -> s
displayEdgeLabel = \case
  EdgeAnd -> "&"
  EdgeOr -> "|"
  EdgeAny -> ";"

-- | Sequential options.
data EdgeSequential
  = -- | Sequential 'and'-edges.
    EdgeSequentialAnd
  | -- | Sequential 'or'-edges.
    EdgeSequentialOr
  | -- | Sequential 'any'-edges.
    EdgeSequentialAny
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | CLI command graph. The default instance is an "edgeless graph", in the
-- sense that all commands are root nodes without any edges, hence normal
-- behavior.
data EdgeArgs
  = -- | Sequential i.e. a linear graph of success edges.
    EdgeArgsSequential EdgeSequential
  | -- | Explicit edges.
    EdgeArgsList Edges
  deriving stock (Eq, Show)

instance Default EdgeArgs where
  def = EdgeArgsList mempty

instance IsList EdgeArgs where
  type Item EdgeArgs = Edge

  fromList = EdgeArgsList . Exts.fromList

  toList (EdgeArgsList xs) = Exts.toList xs
  toList (EdgeArgsSequential _) = error "Called toList on EdgeArgsSequential"

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

instance Pretty CommandGraph where
  pretty c =
    vcat
      [ "graph:",
        prettyGraph,
        "roots: " <> rs
      ]
    where
      rs = hsep (punctuate comma . fmap pretty $ toList (c ^. #roots))

      prettyGraph =
        -- 3. Make the output slightly nicer.
        massageGraph
          -- 2. Use fgl's prettify to create a String rep.
          . G.prettify
          -- 1. Map graph to a less noisy one i.e. remove commands, and
          -- prettyify indices.
          . bimap mapCmd pretty
          $ c
          ^. #graph

      -- G.prettify is pretty good, but we want to do some extra processing
      -- e.g. indent and label each line.
      massageGraph =
        vcat
          . fmap (indent 2 . pretty)
          . T.lines
          . pack

      mapCmd = const (pretty @String " ")

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
  let rootMap = withoutKeys cmdMap nonRoots

  (roots, vs) <- case HMap.toList rootMap of
    [] -> throwText "No root command(s) found! There is probably a cycle."
    (r : rs) -> do
      pure
        ( fmap (view _1) (r :<|| Seq.fromList rs),
          HMap.toList cmdMap
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
      (EdgeArgsSequential s) -> mkSequentialEdges s cmds

    -- nonRoots is all vertices with an in-edge.
    nonRoots :: HashSet Int

    (_, nonRoots) = foldl' mkEdgeMap (HMap.empty, HSet.empty) edges

    mkEdgeMap :: EdgeAcc -> GEdge -> EdgeAcc
    mkEdgeMap (mp, nr) (s, d, _) = case HMap.lookup s mp of
      Nothing -> (HMap.insert s [d] mp, HSet.insert d nr)
      Just es -> (HMap.insert s (d : es) mp, HSet.insert d nr)

    cmdSet = HMap.keysSet cmdMap
    cmdMap =
      HMap.fromList ((\cmd -> (toV $ cmd ^. #index, cmd)) <$> toList cmds)

    withoutKeys :: (Hashable k) => HashMap k v -> HashSet k -> HashMap k v
    withoutKeys m s = m `HMap.difference` HMap.fromList ((,const ()) <$> HSet.toList s)

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
displayCommandIndex = prettyToText

displayVertex :: Node -> Text
displayVertex = displayCommandIndex . Command.Types.fromVertex

toV :: CommandIndex -> Node
toV = Command.Types.toVertex

-- ((out) Edge Map, Non-roots)
type EdgeAcc =
  Tuple2
    (HashMap Int (List Int))
    (HashSet Int)

verifyUniqueEdges :: (HasCallStack, MonadThrow m) => Seq GEdge -> m ()
verifyUniqueEdges edges = case duplicates of
  [] -> pure ()
  es@(_ : _) -> do
    let msg =
          mconcat
            [ "Found multiple edges between the same commands:",
              mconcat $ ("\n  - " <>) . renderDuplicates <$> es
            ]
    throwText msg
  where
    duplicates =
      -- sort for determinism
      L.sort
        . fmap (\((s, d), ls) -> ((s, d), Seq.sort ls))
        . HMap.toList
        . HMap.filter ((> 1) . length)
        . foldl' go HMap.empty
        $ edges

    go :: EdgeDupeAcc -> GEdge -> EdgeDupeAcc
    go found (s, d, l) = HMap.alter combineLabels (s, d) found
      where
        combineLabels Nothing = Just $ Seq.singleton l
        combineLabels (Just ls) = Just $ ls :|> l

    renderDuplicates :: Tuple2 (Tuple2 Node Node) (Seq EdgeLabel) -> Text
    renderDuplicates ((s, d), ls) =
      T.intercalate ", " $ (\l -> renderEdge (s, d, l)) <$> toList ls

    renderEdge :: (Node, Node, EdgeLabel) -> Text
    renderEdge (s, d, l) =
      mconcat
        [ "'",
          showt s,
          " ",
          displayEdgeLabel l,
          " ",
          showt d,
          "'"
        ]

type EdgeNoLabel = Tuple2 Node Node

type EdgeDupeAcc = HashMap EdgeNoLabel (Seq EdgeLabel)

-- | Verifies all commands references in the edges exist.
allEdgesExist :: (HasCallStack, MonadThrow m) => Seq GEdge -> HashSet Int -> m ()
allEdgesExist edges cmds = for_ edges $ \(s, d, _) -> do
  if
    | s `notMember` cmds ->
        throwText $ mkErr s (s, d)
    | d `notMember` cmds ->
        throwText $ mkErr d (s, d)
    | otherwise -> pure ()
  where
    notMember x = not . HSet.member x

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

verifyAllReachable :: (HasCallStack, MonadThrow m) => CommandGraph -> HashSet Int -> m ()
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
    nonReachable = HSet.difference cmdSet reachable
    reachable = reachableFromRoots cdg

-- Dominators seems to do what we want, but if not we can write this manually.
reachableFromRoots :: CommandGraph -> HashSet Node
reachableFromRoots cdg =
  HSet.fromList
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

mkSequentialEdges :: EdgeSequential -> NESeq CommandP1 -> Seq GEdge
mkSequentialEdges eseq =
  dropLast
    . fmap toEdge
    . Seq.sortOn MkCommandOrd
    . NESeq.toSeq
  where
    toEdge (MkCommandP idx _ _) =
      ( toV idx,
        toV $ Command.Types.succ idx,
        label
      )

    label = case eseq of
      EdgeSequentialAnd -> EdgeAnd
      EdgeSequentialOr -> EdgeOr
      EdgeSequentialAny -> EdgeAny

    dropLast Empty = Empty
    dropLast (_ :<| Empty) = Empty
    dropLast (x :<| ys) = x :<| dropLast ys
