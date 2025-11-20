module Shrun.Command
  ( runCommands,
  )
where

import Data.Graph qualified as G
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effects.Concurrent.Async qualified as Async
import Shrun.Command.Types
  ( CommandIndex,
    CommandP1,
    CommandStatus
      ( CommandFailure,
        CommandRunning,
        CommandSuccess,
        CommandWaiting
      ),
    CommandStatusData (CommandStatusUnit, CommandStatusVertex),
  )
import Shrun.Command.Types qualified as Command.Types
import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Configuration.Env.Types
  ( HasCommands (getCommandDepGraph, getCommandStatus),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging,
    HasFileLogging,
    whenDebug,
  )
import Shrun.Data.Text (UnlinedText (UnsafeUnlinedText))
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.MonadRegionLogger
  ( MonadRegionLogger
      ( Region,
        withRegion
      ),
  )
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelDebug, LevelError),
    LogMessage (UnsafeLogMessage),
  )
import Shrun.Logging.Types.Internal (LogMode (LogModeFinish))
import Shrun.Prelude

-- | Responsible for scheduling commands.
runCommands ::
  ( HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadAsync m,
    MonadMVar m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThrow m,
    MonadTime m
  ) =>
  -- | Individual command runner.
  ((HasCallStack) => CommandP1 -> m ()) ->
  m ()
runCommands runner = do
  cdg <- asks getCommandDepGraph
  commandStatuses <- asks getCommandStatus
  let roots = cdg ^. #roots
  vtxSemMap <- mkVertexSemMap cdg
  Async.mapConcurrently_ (runCommand runner cdg commandStatuses vtxSemMap) roots
{-# INLINEABLE runCommands #-}

-- | Builds a map for each Vertex -> MVar. This ensures that we only start
-- each command at most once.
mkVertexSemMap :: (MonadMVar m) => CommandGraph -> m (Map Vertex (MVar ()))
mkVertexSemMap cdg = do
  vs <- for vertices $ \v -> do
    q <- newMVar ()
    pure (v, q)

  pure $ Map.fromList vs
  where
    vertices = G.vertices $ cdg ^. #graph
{-# INLINEABLE mkVertexSemMap #-}

runCommand ::
  forall m env.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadAsync m,
    MonadMVar m,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadThrow m,
    MonadTime m
  ) =>
  -- | Individual command runner.
  ((HasCallStack) => CommandP1 -> m ()) ->
  -- | Command dependency graph.
  CommandGraph ->
  -- | Command status ref.
  TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit))) ->
  -- | Vertex semaphore map, for preventing the same command being kicked off
  -- by multiple commands.
  Map Vertex (MVar ()) ->
  -- | Vertex to run.
  Vertex ->
  m ()
runCommand runner cdg commandStatuses vtxSemMap = go Nothing
  where
    go prevVertex vertex = do
      status <- getPredecessorsStatus cdg commandStatuses vertex
      case status of
        CommandWaiting depV ->
          whenDebug $ do
            logNoRun cdg LevelDebug prevVertex debugMsg (Just depV) vertex
        CommandRunning depV ->
          whenDebug $ do
            logNoRun cdg LevelDebug prevVertex debugMsg (Just depV) vertex
        CommandFailure depV -> logNoRun cdg LevelError prevVertex errMsg (Just depV) vertex
        CommandSuccess -> do
          case Map.lookup vertex vtxSemMap of
            Nothing ->
              throwText
                $ mconcat
                  [ "Could not find vertex ",
                    showt vertex,
                    " in vertex map ",
                    T.intercalate ", " (showt <$> Map.keys vtxSemMap)
                  ]
            Just mvar ->
              -- NOTE: [Command Race]
              --
              -- Suppose we have the following dep graph:
              --
              --   1 -> 3, 2 -> 3
              --
              -- and commmands 1 and 2 take the same amount of time. If both
              -- threads finish 'runner' and call 'mapConcurrently' at the
              -- same time, then we will have two threads corresponding to
              -- 3, and each will notice that all dependent tasks have finished
              -- successfully, hence start the task. That is obviously not
              -- what we want!
              --
              -- Normally, our logic blocks multiple sends like this by
              -- abandoning the attempt if any attempts are still running.
              -- When both finish simultaneously, however, this won't work.
              --
              -- Hence we have a map Vertex -> MVar, and the task can only
              -- start if it can retrieve the MVar. If the MVar is empty,
              -- another thread must have started it.
              --
              -- We never restore the MVar (because a command should only
              -- be run at most once), so the only sensible thing we can do
              -- is exit.
              tryTakeMVar mvar >>= \case
                Nothing ->
                  -- No MVar, print a message and leave.
                  whenDebug $ do
                    logNoRun cdg LevelDebug prevVertex alreadyRunningMsg Nothing vertex
                Just () -> do
                  -- We are not blocked. Run the command and kick off all
                  -- successors.
                  let (cmd, _, edges) = fromV vertex
                  runner cmd
                  Async.mapConcurrently_ (go (Just vertex)) (Command.Types.toVertex <$> edges)

    debugMsg depCmdTxt cmdTxt =
      mconcat
        [ "Command '",
          cmdTxt,
          "' is blocked due to dependency pending: '",
          depCmdTxt,
          "'."
        ]

    errMsg depCmdTxt cmdTxt =
      mconcat
        [ "Not running '",
          cmdTxt,
          "' due to dependency failure: '",
          depCmdTxt,
          "'."
        ]

    alreadyRunningMsg _ cmdTxt =
      mconcat
        [ "Command '",
          cmdTxt,
          "' is already running."
        ]

    fromV = cdg ^. #fromV
{-# INLINEABLE runCommand #-}

getPredecessorsStatus ::
  forall m.
  ( HasCallStack,
    MonadThrow m,
    MonadSTM m
  ) =>
  CommandGraph ->
  TVar (Map CommandIndex (Tuple2 CommandP1 (CommandStatus CommandStatusUnit))) ->
  Vertex ->
  m (CommandStatus CommandStatusVertex)
getPredecessorsStatus cdg commandStatusesRef v = do
  commandStatuses <- readTVarA commandStatusesRef

  let toResult :: Vertex -> m (CommandStatus CommandStatusVertex)
      toResult p =
        let idx = Command.Types.fromVertex p
         in case Map.lookup (Command.Types.fromVertex p) commandStatuses of
              Nothing ->
                throwText
                  $ mconcat
                    [ "Failed searching for command index ",
                      showt (idx ^. #unCommandIndex % #unPositive)
                    ]
              Just (_, status) -> pure $ Command.Types.mapCommandStatus (const p) status

  foldMapA toResult predecessors
  where
    predecessors = findPredecessors cdg v
{-# INLINEABLE getPredecessorsStatus #-}

findPredecessors :: CommandGraph -> Vertex -> List Vertex
findPredecessors cdg v =
  fst
    . foldl' go ([], Set.empty)
    . G.edges
    $ (cdg ^. #graph)
  where
    -- For each s -> t, if t == v and we have not previously found s, add
    -- s as a direct predecessor. Otherwise continue.
    go (acc, found) (s, t)
      | t == v && Set.notMember s found = (s : acc, Set.insert s found)
      | otherwise = (acc, found)

logNoRun ::
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env (Region m),
    HasFileLogging env,
    MonadReader env m,
    MonadRegionLogger m,
    MonadSTM m,
    MonadTime m
  ) =>
  -- | Dependency graph
  CommandGraph ->
  -- | Log level
  LogLevel ->
  -- | The predecessor vertex that started this one. Note that this may
  -- not be the same as mDepVertex.
  Maybe Vertex ->
  -- | Function to construct log message. Parameters are the dep vertex
  -- text and this vertex text.
  (UnlinedText -> UnlinedText -> UnlinedText) ->
  -- | Some predecessor vertex that has some problem.
  Maybe Vertex ->
  -- | Command vertex that will not be run.
  Vertex ->
  m ()
logNoRun cdg lvl mPrevVertex msgFn mDepVertex vertex = do
  commonLogging <- asks getCommonLogging
  let keyHide = commonLogging ^. #keyHide
      (thisCmd, _, _) = fromV vertex

      prevCmd = (\(c, _, _) -> c) . fromV <$> mPrevVertex

      depCmdTxt = case mDepVertex of
        Nothing -> ""
        Just depVertex ->
          let (depCmd, _, _) = fromV depVertex
           in mconcat
                [ "(",
                  vToUnlined depVertex,
                  ") ",
                  Formatting.displayCmd depCmd keyHide
                ]
      cmdTxt = Formatting.displayCmd thisCmd keyHide
      errMsg = msgFn depCmdTxt cmdTxt

      log =
        MkLog
          { cmd = prevCmd,
            msg = coerce errMsg,
            lvl,
            mode = LogModeFinish
          }

  withRegion Linear $ \r -> Logging.putRegionLog r log
  where
    -- Convert back to CommandIndex to match user-supplied value.
    vToUnlined :: Vertex -> UnlinedText
    vToUnlined =
      fromString
        . show
        . view (#unCommandIndex % #unPositive)
        . Command.Types.fromVertex

    fromV = cdg ^. #fromV
{-# INLINEABLE logNoRun #-}
