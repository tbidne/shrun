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

import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Shrun (shrun)
import Shrun.Command.Types
  ( CommandStatus (CommandWaiting),
    CommandStatusMapP (MkCommandStatusMapP),
  )
import Shrun.Configuration qualified as Configuration
import Shrun.Configuration.Args.Parsing qualified as P
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Configuration.Data.LegendKeysCache
  ( KeyCache,
    LegendKeysCache
      ( LegendKeysAdd,
        LegendKeysClear,
        LegendKeysOff,
        LegendKeysWrite
      ),
  )
import Shrun.Configuration.Data.LegendKeysCache qualified as LKC
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
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml (Toml)
import Shrun.Configuration.Toml qualified as Toml
import Shrun.Logging.RegionLogger (RegionLogger)
import Shrun.Prelude

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall notifyEnv r es.
  ( Eq r,
    HasCallStack,
    HasConsoleLogging (Env notifyEnv r) r,
    Concurrent :> es,
    FileReader :> es,
    FileWriter :> es,
    HandleReader :> es,
    HandleWriter :> es,
    Prim :> es,
    Notify notifyEnv :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    PosixFiles :> es,
    PosixSignals :> es,
    Process :> es,
    RegionLogger r :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff es ()
makeEnvAndShrun = withEnv @notifyEnv @r $ \env ->
  runReader env (shrun @(Env notifyEnv r) @notifyEnv @r)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  forall notifyEnv r a es.
  ( HasCallStack,
    Concurrent :> es,
    FileReader :> es,
    FileWriter :> es,
    HandleWriter :> es,
    Prim :> es,
    Notify notifyEnv :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    PosixFiles :> es,
    Terminal :> es
  ) =>
  (Env notifyEnv r -> Eff es a) ->
  Eff es a
withEnv onEnv = getMergedConfig >>= flip fromMergedConfig onEnv

-- | Creates a 'MergedConfig' from CLI args and TOML config.
getMergedConfig ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Prim :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  Eff es (MergedConfig r)
getMergedConfig = do
  xdgState <- getShrunXdgState

  -- Read legend keys from last run, if they exist. We then pass them into
  -- the parser so we get completions.
  keyCache <- readPreviousLegendKeys xdgState
  cwd <- PR.getCurrentDirectory
  let prevKeySet = getCurrentKeys cwd keyCache
      prevKeys = Set.toList prevKeySet

  args <- customExecParser P.parserPrefs (P.parserInfoArgs $ unpack <$> prevKeys)

  let configPaths = (fmap . fmap) TomlOther $ args ^. #configPaths

  tomls <- do
    -- If our configs list contains /any/ Disabled, then the implicit configs
    -- will be ignored, as they are the implicit first elements. Hence we guard
    -- against it to save unnecessary lookups.
    if containsDisabled configPaths
      then pure configPaths
      else (\ps -> (With <$> ps) <> configPaths) <$> findImplicitConfigs cwd

  (tomlPaths, finalToml, cwdToml) <- mergeTomls tomls

  when (args ^. #expandAliases) $ do
    msg <-
      Configuration.tomlToLegendMap finalToml <&> \case
        Nothing -> "<no aliases>"
        Just lm -> Legend.prettyLegendMap lm

    putTextLn $ docToText msg

    throwM ExitSuccess

  merged <- Configuration.mergeConfig args finalToml tomlPaths

  saveLegendKeys xdgState cwd (merged ^. #coreConfig % #legendKeysCache) keyCache finalToml cwdToml

  pure merged
  where
    containsDisabled = L.elem Disabled

data TomlPath
  = TomlCwd OsPath
  | TomlOther OsPath
  deriving stock (Eq, Show)

unTomlPath :: TomlPath -> OsPath
unTomlPath = \case
  TomlCwd p -> p
  TomlOther p -> p

-- | Searches for implicit configs. The list of searched paths are:
--
-- - xdg_config/config.toml
-- - cwd/.shrun.toml
-- - cwd/shrun.toml
findImplicitConfigs ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Seq TomlPath)
findImplicitConfigs cwd = do
  xdgConfig <- getShrunXdgConfig
  let paths =
        TomlOther (xdgConfig </> [osp|config.toml|])
          :<| (TomlOther <$> mkPaths xdgConfig)
          <> (TomlCwd <$> mkPaths cwd)

  xs <- traverse configExists paths
  pure $ catSeqMaybes xs
  where
    mkPaths d =
      [ d </> [osp|.shrun.toml|],
        d </> [osp|shrun.toml|]
      ]

configExists :: (HasCallStack, PathReader :> es) => TomlPath -> Eff es (Maybe TomlPath)
configExists = \case
  TomlCwd p -> (fmap . fmap) TomlCwd (f p)
  TomlOther p -> (fmap . fmap) TomlOther (f p)
  where
    f path = do
      exists <- doesFileExist path
      pure
        $ if exists
          then Just path
          else Nothing

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
  forall notifyEnv es.
  ( HasCallStack,
    FileReader :> es
  ) =>
  Seq (WithDisabled TomlPath) ->
  Eff es (Tuple3 (Seq OsPath) (Toml notifyEnv) (Toml notifyEnv))
mergeTomls tomlPaths = do
  pathsWithTomls <- traverse (\t -> (t,) <$> readConfig (unTomlPath t)) toRead

  let (paths, finalToml) =
        bimap
          -- Reverse toml paths so they are in the original order. No need to reverse
          -- actual Toml files because mergeTomls expects the inverse order.
          (fmap unTomlPath . Seq.reverse)
          Toml.mergeTomls
          . Seq.unzip
          $ pathsWithTomls

      cwdToml =
        Toml.mergeTomls
          . fmap snd
          . Seq.filter (isTomlCwd . fst)
          $ pathsWithTomls

  pure (paths, finalToml, cwdToml)
  where
    toRead = dropAfterDisabled $ Seq.reverse tomlPaths

    dropAfterDisabled Empty = Empty
    dropAfterDisabled (Disabled :<| _) = Empty
    dropAfterDisabled (With f :<| fs) = f :<| dropAfterDisabled fs

    isTomlCwd = \case
      TomlCwd _ -> True
      _ -> False

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
    FileReader :> es
  ) =>
  OsPath ->
  Eff es (Toml notifyEnv)
readConfig fp = do
  contents <- readFileUtf8ThrowM fp
  case decode contents of
    Right cfg -> pure cfg
    Left tomlErr -> throwM $ MkTomlPathError fp tomlErr

fromMergedConfig ::
  ( HasCallStack,
    Concurrent :> es,
    FileWriter :> es,
    HandleWriter :> es,
    Prim :> es,
    Notify notifyEnv :> es,
    PathReader :> es,
    PathWriter :> es,
    PosixFiles :> es,
    Terminal :> es
  ) =>
  MergedConfig notifyEnv ->
  (Env notifyEnv r -> Eff es a) ->
  Eff es a
fromMergedConfig cfg onEnv = do
  when (cfg ^. #dryRun) $ do
    putTextLn
      . prettyToText
      $ cfg
    throwM ExitSuccess

  commandStatusMap <- atomically $ do
    kvs <- for commands $ \cmd -> do
      statusVar <- newTVar' CommandWaiting
      pure (cmd ^. #index, (cmd, statusVar))
    pure $ MkCommandStatusMapP $ HashMap.fromList $ toList kvs

  anyError <- newTVarA' False
  consoleLogQueue <- newTBQueueA 1_000
  hasTimedOut <- newTVarA' False
  timerRegion <- newIORef Nothing

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

getShrunXdgConfig :: (HasCallStack, PathReader :> es) => Eff es OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]

-- | Given the xdg state dir, reads the legend key cache, if it exists.
readPreviousLegendKeys ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  OsPath ->
  Eff es KeyCache
readPreviousLegendKeys xdgState = do
  exists <- PR.doesFileExist keysPath
  if exists
    then do
      -- Don't let a read keys error take down shrun.
      tryMySync (readBinaryFile keysPath) >>= \case
        Left err -> do
          putStrLn $ "Error reading legend keys cache: " <> displayException err
          void $ tryMySync $ PW.removePathForcibly keysPath
          pure mempty
        Right contents -> case Asn.eitherDecodeStrict contents of
          Left jsonErr -> do
            putStrLn $ "Error decoding legend keys json: " <> jsonErr
            void $ tryMySync $ PW.removePathForcibly keysPath
            pure mempty
          Right c -> pure c
    else pure mempty
  where
    keysPath = mkLegendKeysPath xdgState

-- | Saves the legend keys from the currently loaded legend file, depending on
-- the 'LegendKeysCache' parameter.
saveLegendKeys ::
  ( HasCallStack,
    FileWriter :> es,
    PathReader :> es,
    PathWriter :> es
  ) =>
  -- | Shrun xdg state.
  OsPath ->
  -- | Current directory.
  OsPath ->
  -- | Key action.
  LegendKeysCache ->
  -- | Key cache.
  KeyCache ->
  -- | Final toml from this run.
  Toml notifyEnv ->
  -- | Current directory toml, for saving local keys.
  Toml notifyEnv ->
  Eff es ()
saveLegendKeys xdgState cwd cacheAction keyCache finalToml cwdToml =
  case cacheAction of
    -- 1. Do nothing.
    LegendKeysOff -> pure ()
    -- 2. Delete file.
    LegendKeysClear -> PW.removeFileIfExists_ keysPath
    -- 3. Overwrite the previous key file, if it exists. If the current keys are
    --    /not/ equal to the old keys, write them.
    LegendKeysWrite -> do
      let newKeyCache = LKC.mkKeyCache globalKeySet (cwd, localKeySet)
      unless (keyCache == newKeyCache) $ writeKeys newKeyCache
    -- 4. Union the previous and new keys. If the current keys are /not/ a
    --    subset of the previous keys, write the union.
    LegendKeysAdd -> do
      let newKeyCache = LKC.addKeyCache globalKeySet (cwd, localKeySet) keyCache
      unless (keyCache == newKeyCache) $ writeKeys newKeyCache
  where
    toKeyList = toList . fmap (view #key)
    allKeySet = maybe Set.empty (Set.fromList . toKeyList) (finalToml ^. #legend)

    globalKeySet = Set.difference allKeySet localKeySet
    localKeySet = maybe Set.empty (Set.fromList . toKeyList) (cwdToml ^. #legend)

    writeKeys newKeys = do
      let keysBs = BSL.toStrict $ AsnPretty.encodePretty' jsonCfg newKeys
      -- Ensure directory exists.
      PW.createDirectoryIfMissing True xdgState
      writeBinaryFile keysPath keysBs

    keysPath = mkLegendKeysPath xdgState

    jsonCfg =
      AsnPretty.defConfig
        { confIndent = Spaces 2,
          confTrailingNewline = True
        }

getCurrentKeys :: OsPath -> KeyCache -> Set Text
getCurrentKeys p kc = Set.union (kc ^. #global) localKeys
  where
    localKeys = fromMaybe Set.empty $ Map.lookup p (kc ^. #local)

mkLegendKeysPath :: OsPath -> OsPath
mkLegendKeysPath xdgState = xdgState </> [osp|legend-keys.json|]

getShrunXdgState :: (HasCallStack, PathReader :> es) => Eff es OsPath
getShrunXdgState = PR.getXdgState [osp|shrun|]
