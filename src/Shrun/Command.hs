module Shrun.Command
  ( -- * Primary
    runCommands,

    -- * Misc
    PredecessorResult (..),
  )
where

import Data.HashMap.Strict qualified as Map
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
  )
import Shrun.Command.Types qualified as Command.Types
import Shrun.Configuration.Data.Graph
  ( CommandGraph,
    EdgeLabel (EdgeAnd, EdgeAny, EdgeOr),
    Vertex,
  )
import Shrun.Configuration.Data.Graph qualified as Graph
import Shrun.Configuration.Env.Types
  ( HasCommands (getCommandDepGraph, getCommandStatusMap),
    HasCommonLogging (getCommonLogging),
    HasLogging,
  )
import Shrun.Data.Text (UnlinedText (UnsafeUnlinedText))
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (withRegion))
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelError, LevelWarn),
    LogMessage (UnsafeLogMessage),
  )
import Shrun.Logging.Types.Internal (LogMode (LogModeFinish))
import Shrun.Prelude

-- | Responsible for scheduling commands.
runCommands ::
  ( HasCallStack,
    HasCommands env,
    HasLogging env m,
    MonadAsync m,
    MonadEvaluate m,
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
  commandStatusMap <- asks getCommandStatusMap
  let roots = cdg ^. #roots
  vtxSemMap <- mkVertexSemMap cdg
  Async.mapConcurrently_ (runCommand runner cdg commandStatusMap vtxSemMap) roots
{-# INLINEABLE runCommands #-}

-- | Builds a map for each Vertex -> MVar. This ensures that we only start
-- each command at most once.
mkVertexSemMap :: (MonadMVar m) => CommandGraph -> m (HashMap Vertex (MVar ()))
mkVertexSemMap cdg = do
  vs <- for vertices $ \v -> do
    q <- newMVar ()
    pure (v, q)

  pure $ Map.fromList vs
  where
    vertices = Graph.vertices cdg
{-# INLINEABLE mkVertexSemMap #-}

runCommand ::
  forall m env.
  ( HasCallStack,
    HasLogging env m,
    MonadAsync m,
    MonadEvaluate m,
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
  HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus)) ->
  -- | Vertex semaphore map, for preventing the same command being kicked off
  -- by multiple commands.
  HashMap Vertex (MVar ()) ->
  -- | Vertex to run.
  Vertex ->
  m ()
runCommand runner cdg commandStatusMap vtxSemMap = go Nothing
  where
    go prevVertex vertex = do
      -- Check that all predecessor edges have been satisfied.
      status <- getPredecessorsStatus cdg commandStatusMap vertex
      case status of
        PredecessorUnfinished depV ->
          Logging.logDebug $ \lvl -> do
            logCommandAction cdg lvl prevVertex debugMsg (Just depV) vertex
        PredecessorFailure failExpected depV -> do
          if failExpected
            then
              logCommandAction cdg LevelWarn prevVertex failOkMsg (Just depV) vertex
            else
              logCommandAction cdg LevelError prevVertex errMsg (Just depV) vertex
        PredecessorSuccess -> do
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
                  Logging.logDebug $ \lvl -> do
                    logCommandAction cdg lvl prevVertex alreadyRunningMsg Nothing vertex
                Just () -> do
                  -- We are not blocked. Run the command and kick off all
                  -- successors.
                  ctx <- Graph.context cdg vertex
                  let (_, cmd) = Graph.ctxLabVertex ctx
                      outNodes = Graph.ctxOutVertices ctx

                  Logging.logDebug $ \lvl -> do
                    logCommandAction cdg lvl prevVertex startMsg Nothing vertex

                  runner cmd
                  Async.mapConcurrently_ (go (Just vertex)) outNodes

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
        [ "Not starting '",
          cmdTxt,
          "' due to dependency failure: '",
          depCmdTxt,
          "'."
        ]

    failOkMsg depCmdTxt cmdTxt =
      mconcat
        [ "Not starting '",
          cmdTxt,
          "' due to dependency success: '",
          depCmdTxt,
          "'."
        ]

    startMsg _ cmdTxt =
      mconcat
        [ "Starting '",
          cmdTxt,
          "'."
        ]

    alreadyRunningMsg _ cmdTxt =
      mconcat
        [ "Command '",
          cmdTxt,
          "' is already running."
        ]
{-# INLINEABLE runCommand #-}

-- | Given a vertex v, 'PredecessorResult' represents the status of all of
-- its predecessors. The algebra is left-biased for identical constructors,
-- otherwise takes the greatest in
--
-- @
--   PredecessorSuccess < PredecessorUnfinished < PredecessorFailure
-- @
data PredecessorResult
  = -- | Some predecessor unfinished.
    PredecessorUnfinished Vertex
  | -- | All predecessors finished and expectations matched. Note that this
    -- does /not/ necessarily imply all predecessor /commands/ finished
    -- successfully.
    PredecessorSuccess
  | -- | Some predecessor finished but did not match the expectation. The
    -- boolean is True iff failure was expected.
    PredecessorFailure Bool Vertex
  deriving stock (Eq, Show)

instance Semigroup PredecessorResult where
  PredecessorFailure b v <> _ = PredecessorFailure b v
  _ <> PredecessorFailure b v = PredecessorFailure b v
  PredecessorUnfinished v <> _ = PredecessorUnfinished v
  _ <> PredecessorUnfinished v = PredecessorUnfinished v
  PredecessorSuccess <> PredecessorSuccess = PredecessorSuccess

instance Monoid PredecessorResult where
  mempty = PredecessorSuccess

-- | Get result of all predecessor nodes. We only progress if all have finished
-- and each result matches the expectation (e.g. CommandSuccess and EdgeSucces).
getPredecessorsStatus ::
  forall m.
  ( HasCallStack,
    MonadThrow m,
    MonadSTM m
  ) =>
  CommandGraph ->
  HashMap CommandIndex (Tuple2 CommandP1 (TVar CommandStatus)) ->
  Vertex ->
  m PredecessorResult
getPredecessorsStatus cdg commandStatusMap v =
  -- Do all the processing in one transaction. STM inherits its type's monoid
  -- instance, so this satisfies:
  --
  -- 1. Result is fail-fast (Err is a hard stop).
  -- 2. If there are no predecessors we have mempty which is
  --    @Ok PredecessorSuccess@.
  -- 3. Otherwise, results are combined via PredecessorResult's Semigroup,
  --    which is what we want.
  atomically (foldMap toResult predecessors) >>= \case
    Err err -> throwText err
    Ok r -> pure r
  where
    predecessors = Graph.labInVertices cdg v

    toResult :: Tuple2 Vertex EdgeLabel -> STM (Result Text PredecessorResult)
    toResult (p, lbl) =
      let idx = Command.Types.fromVertex p
       in case Map.lookup idx commandStatusMap of
            Nothing ->
              pure
                . Err
                $ mconcat
                  [ "Failed searching for command index ",
                    prettyToText idx
                  ]
            Just (_, statusVar) -> do
              status <- readTVar statusVar
              pure . Ok $ case (status, lbl) of
                (CommandWaiting, _) -> PredecessorUnfinished p
                (CommandRunning _, _) -> PredecessorUnfinished p
                (CommandSuccess, EdgeAnd) -> PredecessorSuccess
                (CommandSuccess, EdgeOr) -> PredecessorFailure True p
                (CommandSuccess, EdgeAny) -> PredecessorSuccess
                (CommandFailure, EdgeAnd) -> PredecessorFailure False p
                (CommandFailure, EdgeOr) -> PredecessorSuccess
                (CommandFailure, EdgeAny) -> PredecessorSuccess
{-# INLINEABLE getPredecessorsStatus #-}

logCommandAction ::
  ( HasCallStack,
    HasLogging env m,
    MonadEvaluate m,
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
logCommandAction cdg lvl mPrevVertex msgFn mDepVertex vertex = do
  commonLogging <- asks getCommonLogging
  thisCmd <- nodeToCommand vertex
  prevCmd <- for mPrevVertex nodeToCommand
  let keyHide = commonLogging ^. #keyHide

  depCmdTxt <- case mDepVertex of
    Nothing -> pure ""
    Just depVertex -> do
      depCmd <- nodeToCommand depVertex
      pure
        $ mconcat
          [ "(",
            vToUnlined depVertex,
            ") ",
            Formatting.displayCmd depCmd keyHide
          ]
  let cmdTxt = Formatting.displayCmd thisCmd keyHide
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
    nodeToCommand = fmap (view _2) . Graph.labVertex cdg

    -- Convert back to CommandIndex to match user-supplied value.
    vToUnlined :: Vertex -> UnlinedText
    vToUnlined =
      fromString
        . unpack
        . prettyToText
        . Command.Types.fromVertex
{-# INLINEABLE logCommandAction #-}
