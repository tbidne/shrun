{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Graph
  ( -- * Args
    CommandGraphArgs (..),
    Edges (..),

    -- * Graph
    CommandGraph (..),
    mkGraph,
    mkTrivialGraph,

    -- * Misc
    CommandGraphF,
  )
where

import Data.Graph qualified as G
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Exts (IsList)
import Shrun.Command.Types
  ( CommandIndex,
    CommandP (MkCommandP),
    CommandP1,
  )
import Shrun.Command.Types qualified as Command.Types
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

type CommandGraphF :: ConfigPhase -> Type
type family CommandGraphF p where
  CommandGraphF ConfigPhaseArgs = WithDisabled CommandGraphArgs
  CommandGraphF ConfigPhaseToml = ()
  CommandGraphF ConfigPhaseMerged = CommandGraph
  CommandGraphF ConfigPhaseEnv = CommandGraph

-- | CLI command graph. The default instance is a "trivial graph", in the
-- sense that all commands are root nodes without any edges, hence normal
-- behavior.
data CommandGraphArgs
  = -- | Sequential i.e. a linear graph.
    CommandGraphArgsSequential
  | -- | Explicit edges.
    CommandGraphArgsEdges Edges
  deriving stock (Eq, Show)

instance Default CommandGraphArgs where
  def = CommandGraphArgsEdges def

-- | Dependency edges are supplied by the user on the CLI.
newtype Edges = MkEdges {unEdges :: List (Tuple2 CommandIndex CommandIndex)}
  deriving newtype (Default, IsList, Monoid, Semigroup)
  deriving stock (Eq, Show)

instance
  ( k ~ An_Iso,
    a ~ List (Tuple2 CommandIndex CommandIndex),
    b ~ List (Tuple2 CommandIndex CommandIndex)
  ) =>
  LabelOptic "unEdges" k Edges Edges a b
  where
  labelOptic = iso (\(MkEdges es) -> es) MkEdges
  {-# INLINE labelOptic #-}

-- | Command dependency graph. Morally, @Vertex + 1 == CommandIndex@.
data CommandGraph = MkCommandGraph
  { -- | Map from a vertex (index) to its command data and edges. This is
    -- a /partial/ function, though we should have the invariant that all
    -- roots and reachable vertices are in the domain.
    fromV :: Vertex -> Tuple3 CommandP1 CommandIndex (List CommandIndex),
    -- | The actual graph.
    graph :: Graph,
    -- | Determines if the Index is in the graph. In practice, this should
    -- never be 'Nothing', as we should only call this with nodes reachable
    -- by the roots.
    idxToV :: CommandIndex -> Maybe Vertex,
    -- | Root commands i.e. have no dependencies.
    roots :: NESeq Vertex
  }

instance Eq CommandGraph where
  x == y = view #graph x == view #graph y

instance Show CommandGraph where
  show = show . view #graph

-- | Creates a command dependency graph from list of dependencies and typed
-- commands.
mkGraph ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  CommandGraphArgs ->
  NESeq CommandP1 ->
  m CommandGraph
mkGraph cdgArgs cmds = do
  -- Verify edges are unique
  verifyUniqueEdges edges

  -- Verify all edges exist.
  allEdgesExist edges cmdSet

  -- Find roots.
  let rootSet = Set.difference cmdSet nonRoots
  roots <- case toList rootSet of
    [] -> throwText "No root command(s) found! There is probably a cycle."
    (r : rs) -> pure $ fmap Command.Types.toVertex $ r :<|| Seq.fromList rs

  let cdg =
        MkCommandGraph
          { fromV,
            graph,
            idxToV,
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
      CommandGraphArgsEdges es -> es
      CommandGraphArgsSequential -> mkSequentialEdges cmds

    (graph, fromV, idxToV) = G.graphFromEdges cmdEdges

    -- command pairs with their edges
    cmdEdges :: List (Tuple3 CommandP1 CommandIndex (List CommandIndex))
    cmdEdges = foldl' mkCmdEdgeList [] cmds

    mkCmdEdgeList :: CmdEdgeAcc -> CommandP1 -> CmdEdgeAcc
    mkCmdEdgeList acc cmd = case HMap.lookup (cmd ^. #index) edgeMap of
      Nothing -> (cmd, cmd ^. #index, []) : acc
      Just es -> (cmd, cmd ^. #index, es) : acc

    -- edgeMap is used for more efficient lookup when constructing the
    -- command edge list
    edgeMap :: HashMap CommandIndex (List CommandIndex)

    -- nonRoots is all vertices with an in-edge.
    nonRoots :: Set CommandIndex

    (edgeMap, nonRoots) =
      foldl' mkEdgeMap (HMap.empty, Set.empty) (edges ^. #unEdges)

    mkEdgeMap :: EdgeAcc -> Tuple2 CommandIndex CommandIndex -> EdgeAcc
    mkEdgeMap (mp, nr) (s, d) = case HMap.lookup s mp of
      Nothing -> (HMap.insert s [d] mp, Set.insert d nr)
      Just es -> (HMap.insert s (d : es) mp, Set.insert d nr)

    -- cmdSet is used to check edge existence
    cmdSet = Set.fromList (view #index <$> toList cmds)

-- | Creates a trivial command dep graph where each command is a
-- disconnected vertex, hence root. This exists to avoid the monad in
-- 'mkGraph', as some uses want purity (defaultConfig...).
-- We /should/ have @mkGraph mempty == mkTrivialGraph@.
mkTrivialGraph :: NESeq CommandP1 -> CommandGraph
mkTrivialGraph cmds =
  MkCommandGraph
    { fromV,
      graph,
      idxToV,
      roots
    }
  where
    (graph, fromV, idxToV) = G.graphFromEdges trivialEdges

    trivialEdges = (\cmd -> (cmd, cmd ^. #index, [])) <$> toList cmds

    roots = Command.Types.toVertex . view #index <$> cmds

type CmdEdgeAcc = List (Tuple3 CommandP1 CommandIndex (List CommandIndex))

type EdgeAcc = Tuple2 (HashMap CommandIndex (List CommandIndex)) (Set CommandIndex)

verifyUniqueEdges :: (HasCallStack, MonadThrow m) => Edges -> m ()
verifyUniqueEdges edges = case toList duplicates of
  [] -> pure ()
  es@(_ : _) -> do
    let msg = "Found duplicates: " <> T.intercalate " " (renderEdge <$> es)
    throwText msg
  where
    (_, duplicates) = foldl' go (Set.empty, Set.empty) (edges ^. #unEdges)

    go (found, dupes) edge
      | Set.member edge found = (found, Set.insert edge dupes)
      | otherwise = (Set.insert edge found, dupes)

    renderEdge (s, t) =
      mconcat
        [ "(",
          displayCommandIndex s,
          ",",
          displayCommandIndex t,
          ")"
        ]

-- | Verifies all commands references in the edges exist.
allEdgesExist :: (HasCallStack, MonadThrow m) => Edges -> Set CommandIndex -> m ()
allEdgesExist edges cmds = do
  for_ (edges ^. #unEdges) $ \e@(s, d) -> do
    if
      | s `notMember` cmds ->
          throwText $ mkErr s e
      | d `notMember` cmds ->
          throwText $ mkErr d e
      | otherwise -> pure ()
  where
    notMember x = not . Set.member x

    mkErr x (s, d) =
      mconcat
        [ "Command index ",
          displayCommandIndex x,
          " in dependency ",
          displayCommandIndex s,
          " -> ",
          displayCommandIndex d,
          " does not exist."
        ]

verifyAllReachable :: (HasCallStack, MonadThrow m) => CommandGraph -> Set CommandIndex -> m ()
verifyAllReachable cdg cmdSet = case toList nonReachable of
  [] -> pure ()
  xs@(_ : _) -> do
    let msg =
          mconcat
            [ "The following commands are not reachable: ",
              T.intercalate ", " (displayCommandIndex <$> xs),
              ". There is probably a cycle."
            ]
    throwText msg
  where
    nonReachable = Set.difference cmdSet (Set.map Command.Types.fromVertex reachable)

    reachable = reachableFromRoots cdg

reachableFromRoots :: CommandGraph -> Set Vertex
reachableFromRoots cdg =
  Set.fromList
    . (G.reachable graph <=< toList)
    $ cdg
    ^. #roots
  where
    graph = cdg ^. #graph

-- | Verifies that no cycles exist.
verifyNoCycles :: (HasCallStack, MonadThrow m) => CommandGraph -> m ()
verifyNoCycles cdg = traverse_ (traverseVertex cdg) (cdg ^. #roots)

traverseVertex :: forall m. (HasCallStack, MonadThrow m) => CommandGraph -> Vertex -> m ()
traverseVertex cdg = go (HSet.empty, [])
  where
    fromV :: Vertex -> (CommandP1, Vertex, [Vertex])
    fromV =
      (\(a, b, c) -> (a, Command.Types.toVertex b, Command.Types.toVertex <$> c))
        . (cdg ^. #fromV)

    go :: CycleAcc -> Vertex -> m ()
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
          let (_, _, es) = fromV v
          traverse_ (go (HSet.insert v foundVs, v : path)) es

    renderPath = T.intercalate " -> " . fmap displayVertex

type CycleAcc = (HashSet Vertex, [Vertex])

mkSequentialEdges :: NESeq CommandP1 -> Edges
mkSequentialEdges =
  MkEdges
    . dropLast
    . fmap toEdge
    . L.sort
    . toList
  where
    toEdge (MkCommandP idx _ _) = (idx, Command.Types.succ idx)

    dropLast [] = []
    dropLast [_] = []
    dropLast (x : ys) = x : dropLast ys

instance
  ( k ~ A_Lens,
    a ~ (Vertex -> (CommandP1, CommandIndex, [CommandIndex])),
    b ~ (Vertex -> (CommandP1, CommandIndex, [CommandIndex]))
  ) =>
  LabelOptic "fromV" k CommandGraph CommandGraph a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandGraph a1 a2 a3 a4) ->
        fmap
          (\b -> MkCommandGraph b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ Graph,
    b ~ Graph
  ) =>
  LabelOptic "graph" k CommandGraph CommandGraph a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandGraph a1 a2 a3 a4) ->
        fmap
          (\b -> MkCommandGraph a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ (CommandIndex -> Maybe Vertex),
    b ~ (CommandIndex -> Maybe Vertex)
  ) =>
  LabelOptic "idxToV" k CommandGraph CommandGraph a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandGraph a1 a2 a3 a4) ->
        fmap
          (\b -> MkCommandGraph a1 a2 b a4)
          (f a3)
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
      $ \f (MkCommandGraph a1 a2 a3 a4) ->
        fmap
          (\b -> MkCommandGraph a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

displayCommandIndex :: CommandIndex -> Text
displayCommandIndex = showt . view (#unCommandIndex % #unPositive)

displayVertex :: Vertex -> Text
displayVertex = displayCommandIndex . Command.Types.fromVertex
