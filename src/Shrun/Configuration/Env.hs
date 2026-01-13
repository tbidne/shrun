{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functions for creating 'Env' from CLI/Toml configuration.
module Shrun.Configuration.Env
  ( -- * Running with Env
    withEnv,
    makeEnvAndShrun,

    -- * Misc
    TomlPathError (..),
    getMergedConfig,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Shrun (runShellT, shrun)
import Shrun.Command.Types (CommandStatus (CommandWaiting))
import Shrun.Configuration (mergeConfig)
import Shrun.Configuration.Args.Parsing qualified as P
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Configuration.Data.LegendKeysCache
  ( LegendKeysCache
      ( LegendKeysAdd,
        LegendKeysClear,
        LegendKeysOff,
        LegendKeysWrite
      ),
  )
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.WithDisabled (WithDisabled (Disabled, With))
import Shrun.Configuration.Env.Types
  ( CommandCleanup
      ( MkCommandCleanup,
        findPidsExe,
        killPidsExe
      ),
    Env
      ( MkEnv,
        anyError,
        commandCleanup,
        commandGraph,
        commandStatusMap,
        commands,
        config,
        consoleLogQueue,
        hasTimedOut,
        timerRegion
      ),
    HasConsoleLogging,
  )
import Shrun.Configuration.Toml (Toml)
import Shrun.Configuration.Toml qualified as Toml
import Shrun.Logging.MonadRegionLogger (MonadRegionLogger (Region))
import Shrun.Notify.DBus (MonadDBus)
import Shrun.Prelude
import Shrun.ShellT (ShellT)

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall m r.
  ( HasCallStack,
    HasConsoleLogging (Env r) (Region (ShellT (Env r) m)),
    MonadAsync m,
    MonadDBus m,
    MonadEvaluate m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixSignals m,
    MonadProcess m,
    MonadMask m,
    MonadMVar m,
    MonadSTM m,
    MonadRegionLogger m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
makeEnvAndShrun = withEnv @m @r (runShellT shrun)
{-# INLINEABLE makeEnvAndShrun #-}

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  forall m r a.
  ( HasCallStack,
    MonadCatch m,
    MonadDBus m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  (Env r -> m a) ->
  m a
withEnv onEnv = getMergedConfig >>= flip fromMergedConfig onEnv
{-# INLINEABLE withEnv #-}

-- | Creates a 'MergedConfig' from CLI args and TOML config.
getMergedConfig ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  m MergedConfig
getMergedConfig = do
  xdgState <- getShrunXdgState

  -- Read legend keys from last run, if they exist. We then pass them into
  -- the parser so we get completions.
  prevKeys <- readPreviousLegendKeys xdgState

  args <- customExecParser P.parserPrefs (P.parserInfoArgs $ unpack <$> prevKeys)

  let configPaths = args ^. #configPaths

  tomls <- do
    -- If our configs list contains /any/ Disabled, then the implicit configs
    -- will be ignored, as they are the implicit first elements. Hence we guard
    -- against it to save unnecessary lookups.
    if containsDisabled configPaths
      then pure configPaths
      else (\ps -> (With <$> ps) <> configPaths) <$> findImplicitConfigs

  (tomlPaths, finalToml) <- mergeTomls tomls

  merged <- mergeConfig args finalToml tomlPaths

  saveLegendKeys xdgState (merged ^. #coreConfig % #legendKeysCache) prevKeys finalToml

  pure merged
  where
    containsDisabled = L.elem Disabled
{-# INLINEABLE getMergedConfig #-}

-- | Searches for implicit configs. The list of searched paths are:
--
-- - xdg_config/config.toml
-- - cwd/.shrun.toml
-- - cwd/shrun.toml
findImplicitConfigs ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  m (Seq OsPath)
findImplicitConfigs = do
  xdgConfig <- getShrunXdgConfig
  cwd <- PR.getCurrentDirectory
  let paths =
        [ xdgConfig </> [osp|config.toml|],
          cwd </> [osp|.shrun.toml|],
          cwd </> [osp|shrun.toml|]
        ]

  xs <- traverse configExists paths
  pure $ catSeqMaybes xs
{-# INLINEABLE findImplicitConfigs #-}

configExists :: (HasCallStack, MonadPathReader m) => OsPath -> m (Maybe OsPath)
configExists p = do
  exists <- doesFileExist p
  pure
    $ if exists
      then Just p
      else Nothing
{-# INLINEABLE configExists #-}

-- | Merges several toml files together.
--
-- NOTE: [Toml order]
--
-- @
--   xdg, t1, t2, ..., tn
-- @
--
-- Where the xdg is the first toml (if it exists), and the rest are given
-- on the CLI. We want the semantics to favor the RHS when there are conflicts.
-- In particular, if some @tk == disabled@, we want all @ti, i < k@ to be
-- disabled.
--
-- Hence we reverse the list to
--
-- @
-- tn, ..., t2, t1, xdg
-- @
--
-- Then drop everything after finding a disabled config. Note that we do
-- /not/ restore the original order (i.e. reverse again). Why? Because our
-- semigroups are left-biased, and we want @tk@ to override @ti@ whenever
-- @i < k@. Hence we can leave the reverse order and foldr.
mergeTomls ::
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  Seq (WithDisabled OsPath) ->
  m (Tuple2 (Seq OsPath) Toml)
mergeTomls =
  -- Reverse toml paths so they are in the original order. No need to reverse
  -- actual Toml files because mergeTomls expects the inverse order.
  fmap (bimap Seq.reverse Toml.mergeTomls . Seq.unzip)
    . traverse (\t -> (t,) <$> readConfig t)
    . dropAfterDisabled
    . Seq.reverse
  where
    dropAfterDisabled Empty = Empty
    dropAfterDisabled (Disabled :<| _) = Empty
    dropAfterDisabled (With f :<| fs) = f :<| dropAfterDisabled fs

data TomlPathError = MkTomlPathError OsPath TOMLError
  deriving stock (Show)

instance Exception TomlPathError where
  displayException (MkTomlPathError p err) =
    mconcat
      [ "Toml error in '",
        decodeLenient p,
        "': ",
        displayException err
      ]

readConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Toml
readConfig fp = do
  contents <- readFileUtf8ThrowM fp
  case decode contents of
    Right cfg -> pure cfg
    Left tomlErr -> throwM $ MkTomlPathError fp tomlErr

fromMergedConfig ::
  ( HasCallStack,
    MonadCatch m,
    MonadDBus m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  MergedConfig ->
  (Env r -> m a) ->
  m a
fromMergedConfig cfg onEnv = do
  when (cfg ^. #dryRun) $ do
    putTextLn
      . prettyToText
      $ cfg
    throwM ExitSuccess

  commandStatusMap <- atomically $ do
    kvs <- for commands $ \cmd -> do
      statusVar <- newTVar CommandWaiting
      pure (cmd ^. #index, (cmd, statusVar))
    pure $ Map.fromList $ toList kvs

  anyError <- newTVarA False
  consoleLogQueue <- newTBQueueA 1_000
  hasTimedOut <- newTVarA False
  timerRegion <- newIORef' Nothing

  mKillExe <- mFindExe [osp|kill|]
  mPGrepExe <- mFindExe [osp|pgrep|]

  let commandCleanup = do
        findPidsExe <- mPGrepExe
        killPidsExe <- mKillExe
        pure
          $ MkCommandCleanup
            { findPidsExe,
              killPidsExe
            }

  CoreConfig.withCoreEnv (cfg ^. #coreConfig) $ \coreConfigEnv -> do
    let env =
          MkEnv
            { config = coreConfigEnv,
              anyError,
              commandStatusMap,
              consoleLogQueue,
              commandGraph,
              commands,
              commandCleanup,
              hasTimedOut,
              timerRegion
            }

    onEnv env
  where
    commands = cfg ^. #commands
    commandGraph = cfg ^. #commandGraph

    mFindExe p = do
      tryMySync (PR.findExecutable p) <&> \case
        Left _ -> Nothing
        Right mPath -> mPath >>= decodeThrowM
{-# INLINEABLE fromMergedConfig #-}

getShrunXdgConfig :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]
{-# INLINEABLE getShrunXdgConfig #-}

-- | Given the xdg state dir, reads the legend key cache, if it exists.
readPreviousLegendKeys ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  OsPath ->
  m (List Text)
readPreviousLegendKeys xdgState = do
  exists <- PR.doesFileExist keysPath
  if exists
    then do
      -- Don't let a read keys error take down shrun.
      tryMySync (readFileUtf8ThrowM keysPath) >>= \case
        Left err -> do
          putStrLn $ "Error reading legend keys cache: " <> displayException err
          void $ tryMySync $ PW.removePathForcibly keysPath
          pure []
        Right contents -> pure $ T.lines $ T.strip contents
    else pure []
  where
    keysPath = mkLegendKeysPath xdgState
{-# INLINEABLE readPreviousLegendKeys #-}

-- | Saves the legend keys from the currently loaded legend file, depending on
-- the 'LegendKeysCache' parameter.
saveLegendKeys ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Shrun xdg state.
  OsPath ->
  -- | Key action.
  LegendKeysCache ->
  -- | Keys from last run.
  List Text ->
  -- | Toml from this run.
  Toml ->
  m ()
saveLegendKeys xdgState cache prevKeysList toml =
  case cache of
    -- 1. Do nothing.
    LegendKeysOff -> pure ()
    -- 2. Delete file.
    LegendKeysClear -> PW.removeFileIfExists_ keysPath
    -- 3. Overwrite the previous key file, if it exists. If the current keys are
    --    /not/ equal to the old keys, write them.
    LegendKeysWrite ->
      unless (prevKeySet == currKeySet) $ writeKeys currKeySet
    -- 4. Union the previous and new keys. If the current keys are /not/ a
    --    subset of the previous keys, write the union.
    LegendKeysAdd ->
      unless currIsSubset $ writeKeys (Set.union prevKeySet currKeySet)
  where
    toKeyList = toList . fmap (view #key)
    prevKeySet = Set.fromList prevKeysList
    currKeySet = maybe Set.empty (Set.fromList . toKeyList) (toml ^. #legend)

    currIsSubset = currKeySet `Set.isSubsetOf` prevKeySet

    writeKeys newKeys = do
      let allKeys = L.sort $ toList newKeys
      -- Ensure directory exists.
      PW.createDirectoryIfMissing True xdgState
      writeFileUtf8 keysPath (T.intercalate "\n" allKeys)

    keysPath = mkLegendKeysPath xdgState
{-# INLINEABLE saveLegendKeys #-}

mkLegendKeysPath :: OsPath -> OsPath
mkLegendKeysPath xdgState = xdgState </> [osp|legend-keys.txt|]

getShrunXdgState :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgState = PR.getXdgState [osp|shrun|]
