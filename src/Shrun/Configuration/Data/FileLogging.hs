{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.FileLogging
  ( FileLogInitP (..),
    FileLogOpened (..),
    FileLoggingP (..),
    FileLoggingArgs,
    FileLoggingToml,
    FileLoggingMerged,
    FileLoggingEnv,
    DeleteOnSuccessSwitch (..),
    mergeFileLogging,
    withFileLoggingEnv,
  )
where

import Data.Bytes
  ( FloatingFormatter (MkFloatingFormatter),
    Normalize (normalize),
    formatSized,
    sizedFormatterNatural,
  )
import Data.Text qualified as T
import Data.Word (Word16)
import Effects.FileSystem.HandleWriter (MonadHandleWriter (withBinaryFile), die)
import Effects.FileSystem.PathWriter (MonadPathWriter (createDirectoryIfMissing))
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseDisabledMaybeF,
    ConfigPhaseF,
    LineTruncF,
    SwitchF,
    parseSwitch,
  )
import Shrun.Configuration.Data.FileLogging.FileMode
  ( FileMode
      ( FileModeAppend,
        FileModeRename,
        FileModeWrite
      ),
  )
import Shrun.Configuration.Data.FileLogging.FilePathDefault
  ( FilePathDefault
      ( FPDefault,
        FPManual
      ),
  )
import Shrun.Configuration.Data.FileLogging.FileSizeMode
  ( FileSizeMode
      ( FileSizeModeDelete,
        FileSizeModeNothing,
        FileSizeModeWarn
      ),
  )
import Shrun.Configuration.Data.StripControl (FileLogStripControl)
import Shrun.Configuration.Data.Truncation
  ( DetectResult,
    TruncRegion (TruncCommandName),
    Truncation,
    decodeCommandNameTrunc,
    decodeLineTrunc,
    mergeLineTrunc,
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<|?|>))
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Logging.Types (FileLog)
import Shrun.Prelude
import Shrun.Utils qualified as Utils
import System.OsPath qualified as OsPath

-- | Switch for deleting the log file upon success.
newtype DeleteOnSuccessSwitch = MkDeleteOnSuccessSwitch Bool
  deriving stock (Eq, Show)
  deriving newtype (Bounded, Enum)
  deriving (Pretty) via PrettySwitch

instance Default DeleteOnSuccessSwitch where
  def = MkDeleteOnSuccessSwitch False

instance DecodeTOML DeleteOnSuccessSwitch where
  tomlDecoder = MkDeleteOnSuccessSwitch <$> (tomlDecoder >>= parseSwitch)

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic
    "unDeleteOnSuccessSwitch"
    k
    DeleteOnSuccessSwitch
    DeleteOnSuccessSwitch
    a
    b
  where
  labelOptic = iso (\(MkDeleteOnSuccessSwitch b) -> b) MkDeleteOnSuccessSwitch
  {-# INLINE labelOptic #-}

-- NOTE: [Args vs. Toml mandatory fields]
--
-- Some "aggregate types" (e.g. FileLogging, NotifyConfig) are optional
-- on the config itself i.e. they are surrounded by Maybe. This is opposed
-- to normal types (e.g. ConsoleLogging) that always exist.
--
-- We do this because it makes it easier to verify whether certain actions
-- should be "on" at all. For instance, if file-logging has not been set
-- (the user has not given us any 'path'), then not only do we not want to
-- log anything (fine, the field is Nothing), but we also do not want to
-- send any log messages to the queue, since that is just wasted work.
--
-- Similarly, if the user has not set any notify actions, then we do not
-- want to send any to its queue.
--
-- By surrounding the entire config in a maybe, we do not need to worry about
-- checking any implicit invariants ("only send notif to queue if some
-- action is set").
--
-- This generally means that the important fields (file-log path,
-- notify action), will be required on if the config exists.

-- | File logging's path is only optional for the Args and Toml. For merged,
-- it must be present if file logging is active.
type FileLogPathF :: ConfigPhase -> Type
type family FileLogPathF p where
  FileLogPathF ConfigPhaseArgs = Maybe (WithDisabled FilePathDefault)
  FileLogPathF ConfigPhaseToml = Maybe (WithDisabled FilePathDefault)
  FileLogPathF ConfigPhaseMerged = FilePathDefault

-- | Initial file log params, for usage before we create the final Env.
data FileLogInitP p = MkFileLogInitP
  { -- | Optional path to log file.
    path :: FileLogPathF p,
    -- | Mode to use with the file log.
    mode :: ConfigPhaseF p FileMode,
    -- | Threshold for when we should warn about the log file size.
    sizeMode :: ConfigPhaseF p FileSizeMode
  }

instance
  ( k ~ A_Lens,
    a ~ FileLogPathF p,
    b ~ FileLogPathF p
  ) =>
  LabelOptic "path" k (FileLogInitP p) (FileLogInitP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLogInitP a1 a2 a3) ->
        fmap
          (\b -> MkFileLogInitP b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p FileMode,
    b ~ ConfigPhaseF p FileMode
  ) =>
  LabelOptic "mode" k (FileLogInitP p) (FileLogInitP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLogInitP a1 a2 a3) ->
        fmap
          (\b -> MkFileLogInitP a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p FileSizeMode,
    b ~ ConfigPhaseF p FileSizeMode
  ) =>
  LabelOptic "sizeMode" k (FileLogInitP p) (FileLogInitP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLogInitP a1 a2 a3) ->
        fmap
          (\b -> MkFileLogInitP a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance Semigroup FileLogInitToml where
  l <> r =
    MkFileLogInitP
      { path = l ^. #path <|> r ^. #path,
        mode = l ^. #mode <|> r ^. #mode,
        sizeMode = l ^. #sizeMode <|> r ^. #sizeMode
      }

instance Monoid FileLogInitToml where
  mempty =
    MkFileLogInitP
      { path = Nothing,
        mode = Nothing,
        sizeMode = Nothing
      }

type FileLogInitArgs = FileLogInitP ConfigPhaseArgs

type FileLogInitToml = FileLogInitP ConfigPhaseToml

type FileLogInitMerged = FileLogInitP ConfigPhaseMerged

deriving stock instance Eq FileLogInitArgs

deriving stock instance Show FileLogInitArgs

deriving stock instance Eq FileLogInitToml

deriving stock instance Show FileLogInitToml

deriving stock instance Eq FileLogInitMerged

deriving stock instance Show FileLogInitMerged

-- Only Default instance is for Args, since others require the Path.
instance Default FileLogInitArgs where
  def =
    MkFileLogInitP
      { path = Nothing,
        mode = Nothing,
        sizeMode = Nothing
      }

instance DecodeTOML FileLogInitToml where
  tomlDecoder =
    MkFileLogInitP
      <$> decodeFileLogging
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

decodeFileLogging :: Decoder (Maybe (WithDisabled FilePathDefault))
decodeFileLogging = getFieldOptWith tomlDecoder "path"

decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"

-- | Params after we have opened the file for logging.
data FileLogOpened = MkFileLogOpened
  { -- | File handle.
    handle :: ~Handle,
    -- | File log queue.
    queue :: ~(TBQueue FileLog)
  }

instance
  ( k ~ A_Lens,
    a ~ Handle,
    b ~ Handle
  ) =>
  LabelOptic "handle" k FileLogOpened FileLogOpened a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLogOpened a1 a2) ->
        fmap
          (\b -> MkFileLogOpened b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TBQueue FileLog,
    b ~ TBQueue FileLog
  ) =>
  LabelOptic "queue" k FileLogOpened FileLogOpened a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLogOpened a1 a2) ->
        fmap
          (\b -> MkFileLogOpened a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

type FileLogFileF :: ConfigPhase -> Type
type family FileLogFileF p where
  FileLogFileF ConfigPhaseArgs = FileLogInitP ConfigPhaseArgs
  FileLogFileF ConfigPhaseToml = FileLogInitP ConfigPhaseToml
  FileLogFileF ConfigPhaseMerged = FileLogInitP ConfigPhaseMerged
  FileLogFileF ConfigPhaseEnv = FileLogOpened

-- | Holds file logging config.
type FileLoggingP :: ConfigPhase -> Type
data FileLoggingP p = MkFileLoggingP
  { -- | File-related params.
    file :: FileLogFileF p,
    -- | The max number of command characters to display in the file logs.
    commandNameTrunc :: ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName),
    -- | If active, deletes the log file upon success.
    deleteOnSuccess :: SwitchF p DeleteOnSuccessSwitch,
    -- | Determines to what extent we should remove control characters
    -- from file logs.
    lineTrunc :: LineTruncF p,
    -- | Strip control
    stripControl :: ConfigPhaseF p FileLogStripControl
  }

instance
  ( k ~ A_Lens,
    a ~ FileLogFileF p,
    b ~ FileLogFileF p
  ) =>
  LabelOptic "file" k (FileLoggingP p) (FileLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkFileLoggingP b a2 a3 a4 a5)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName),
    b ~ ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName)
  ) =>
  LabelOptic "commandNameTrunc" k (FileLoggingP p) (FileLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkFileLoggingP a1 b a3 a4 a5)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ SwitchF p DeleteOnSuccessSwitch,
    b ~ SwitchF p DeleteOnSuccessSwitch
  ) =>
  LabelOptic "deleteOnSuccess" k (FileLoggingP p) (FileLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkFileLoggingP a1 a2 b a4 a5)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LineTruncF p,
    b ~ LineTruncF p
  ) =>
  LabelOptic "lineTrunc" k (FileLoggingP p) (FileLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkFileLoggingP a1 a2 a3 b a5)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p FileLogStripControl,
    b ~ ConfigPhaseF p FileLogStripControl
  ) =>
  LabelOptic "stripControl" k (FileLoggingP p) (FileLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkFileLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkFileLoggingP a1 a2 a3 a4 b)
          (f a5)
  {-# INLINE labelOptic #-}

instance Semigroup FileLoggingToml where
  l <> r =
    MkFileLoggingP
      { file = l ^. #file <> r ^. #file,
        commandNameTrunc = l ^. #commandNameTrunc <|> r ^. #commandNameTrunc,
        deleteOnSuccess = l ^. #deleteOnSuccess <|> r ^. #deleteOnSuccess,
        lineTrunc = l ^. #lineTrunc <|> r ^. #lineTrunc,
        stripControl = l ^. #stripControl <|> r ^. #stripControl
      }

instance Monoid FileLoggingToml where
  mempty =
    MkFileLoggingP
      { file = mempty,
        commandNameTrunc = Nothing,
        deleteOnSuccess = Nothing,
        lineTrunc = Nothing,
        stripControl = Nothing
      }

instance Pretty FileLoggingMerged where
  pretty c =
    vcat
      [ "command-name-trunc: " <> prettyMaybe (c ^. #commandNameTrunc),
        "delete-on-success: " <> pretty (c ^. #deleteOnSuccess),
        "line-trunc: " <> prettyMaybe (c ^. #lineTrunc),
        "mode: " <> pretty (c ^. #file % #mode),
        "path: " <> pretty (c ^. #file % #path),
        "size-mode: " <> pretty (c ^. #file % #sizeMode),
        "strip-control: " <> pretty (c ^. #stripControl)
      ]

type FileLoggingArgs = FileLoggingP ConfigPhaseArgs

type FileLoggingToml = FileLoggingP ConfigPhaseToml

type FileLoggingMerged = FileLoggingP ConfigPhaseMerged

type FileLoggingEnv = FileLoggingP ConfigPhaseEnv

deriving stock instance Eq (FileLoggingP ConfigPhaseArgs)

deriving stock instance Show (FileLoggingP ConfigPhaseArgs)

deriving stock instance Eq (FileLoggingP ConfigPhaseToml)

deriving stock instance Show (FileLoggingP ConfigPhaseToml)

deriving stock instance Eq (FileLoggingP ConfigPhaseMerged)

deriving stock instance Show (FileLoggingP ConfigPhaseMerged)

instance Default FileLoggingArgs where
  def =
    MkFileLoggingP
      { file = def,
        commandNameTrunc = Nothing,
        deleteOnSuccess = Nothing,
        lineTrunc = Nothing,
        stripControl = Nothing
      }

-- | Merges args and toml configs.
mergeFileLogging ::
  ( HasCallStack,
    MonadCatch m,
    MonadIORef m,
    MonadTerminal m
  ) =>
  IORef DetectResult ->
  FileLoggingArgs ->
  Maybe FileLoggingToml ->
  m (Maybe FileLoggingMerged)
mergeFileLogging detectRef args mToml = for mPath $ \path -> do
  let toml = fromMaybe defaultToml mToml

  lineTrunc <-
    mergeLineTrunc False detectRef (args ^. #lineTrunc) (toml ^. #lineTrunc)

  pure
    $ MkFileLoggingP
      { file =
          MkFileLogInitP
            { path,
              mode =
                (args ^. #file % #mode) <.> (toml ^. #file % #mode),
              sizeMode =
                (args ^. #file % #sizeMode) <.> (toml ^. #file % #sizeMode)
            },
        commandNameTrunc =
          (args ^. #commandNameTrunc) <|?|> (toml ^. #commandNameTrunc),
        deleteOnSuccess =
          args
            ^. #deleteOnSuccess
            <.> (toml ^. #deleteOnSuccess),
        lineTrunc,
        stripControl =
          (args ^. #stripControl) <.> (toml ^. #stripControl)
      }
  where
    -- NOTE: [Config two-part pattern matching]
    --
    -- Why do we pattern match here and in the main body of mergeFileLogging,
    -- rather than just once? If we did all of it in the body we'd have logic
    -- like:
    --
    --     if Disabled and No Toml
    --       Nothing
    --     else if No args and No Toml
    --       Nothing
    --     else if Args and No Toml
    --       Just fileLogging ...
    --     else if No Args and Toml
    --       Just fileLogging ...
    --     else Args and Toml
    --       Just fileLogging ..
    --
    -- That is, we'd repeat the "Just fileLogging" step several types, and
    -- since it is already quite wordy, readability suffers. It is easier to
    -- reduce the pattern matching down to a "go no-go" switch first, then
    -- make the fileLogging based on that.
    mPath :: Maybe FilePathDefault
    mPath = args ^. #file % #path <|?|> (mToml ^? Utils.surroundJust (#file % #path))
{-# INLINEABLE mergeFileLogging #-}

instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingP
      <$> tomlDecoder
      <*> decodeCommandNameTrunc
      <*> decodeFileDeleteOnSuccess
      <*> decodeLineTrunc
      <*> decodeFileLogStripControl

decodeFileDeleteOnSuccess :: Decoder (Maybe DeleteOnSuccessSwitch)
decodeFileDeleteOnSuccess = getFieldOptWith tomlDecoder "delete-on-success"

decodeFileLogStripControl :: Decoder (Maybe FileLogStripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

type MLogging = Maybe (Tuple3 FileLoggingMerged Handle (TBQueue FileLog))

-- | Given merged FileLogging config, constructs a FileLoggingEnv and calls
-- the continuation.
withFileLoggingEnv ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Maybe FileLoggingMerged ->
  (Maybe FileLoggingEnv -> m a) ->
  m a
withFileLoggingEnv mFileLogging onFileLoggingEnv = do
  let mkEnv :: MLogging -> Maybe FileLoggingEnv
      mkEnv Nothing = Nothing
      mkEnv (Just (fl, h, q)) =
        Just
          $ MkFileLoggingP
            { file =
                MkFileLogOpened
                  { handle = h,
                    queue = q
                  },
              commandNameTrunc = fl ^. #commandNameTrunc,
              lineTrunc = fl ^. #lineTrunc,
              deleteOnSuccess = fl ^. #deleteOnSuccess,
              stripControl = fl ^. #stripControl
            }

  withMLogging mFileLogging (onFileLoggingEnv . mkEnv)
{-# INLINEABLE withFileLoggingEnv #-}

withMLogging ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Maybe FileLoggingMerged ->
  (MLogging -> m a) ->
  m a
-- 1. No file logging
withMLogging Nothing onLogging = onLogging Nothing
-- 2. Use the default path.
withMLogging (Just fileLogging) onLogging = do
  let fileMode = fileLogging ^. #file % #mode
      ioMode = case fileMode of
        FileModeAppend -> AppendMode
        FileModeRename -> WriteMode
        FileModeWrite -> WriteMode

  fp <- case fileLogging ^. #file % #path of
    FPDefault -> do
      stateDir <- getShrunXdgState
      let fp = stateDir </> [osp|shrun.log|]
      stateExists <- doesDirectoryExist stateDir
      unless stateExists (createDirectoryIfMissing True stateDir)
      pure fp
    FPManual fp -> pure fp

  uniqFp <- createLogFile fileMode fp
  handleLogFileSize (fileLogging ^. #file % #sizeMode) uniqFp
  fileQueue <- newTBQueueA 1000

  result <-
    withBinaryFile uniqFp ioMode $ \h ->
      onLogging (Just (fileLogging, h, fileQueue))

  -- If the above command succeeded and deleteOnSuccess is true, delete the
  -- log file. Otherwise we will not reach here due to withBinaryFile
  -- rethrowing an exception, so the file will not be deleted.
  when (fileLogging ^. #deleteOnSuccess % #unDeleteOnSuccessSwitch)
    $ removeFileIfExists_ uniqFp

  pure result
{-# INLINEABLE withMLogging #-}

handleLogFileSize ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  FileSizeMode ->
  OsPath ->
  m ()
handleLogFileSize fileSizeMode fp = do
  fileSize <- MkBytes @B . unsafeConvertIntegral <$> getFileSize fp
  case fileSizeMode of
    FileSizeModeWarn warnSize ->
      when (fileSize > warnSize)
        $ putTextLn
        $ sizeWarning warnSize fileSize
    FileSizeModeDelete delSize ->
      when (fileSize > delSize) $ do
        putTextLn $ sizeWarning delSize fileSize <> " Deleting log."
        removeFile fp
    FileSizeModeNothing -> pure ()
  where
    sizeWarning warnSize fileSize =
      mconcat
        [ "Warning: log file '",
          T.pack $ decodeLenient fp,
          "' has size: ",
          formatBytes fileSize,
          ", but specified threshold is: ",
          formatBytes warnSize,
          "."
        ]

    formatBytes =
      formatSized (MkFloatingFormatter (Just 2)) sizedFormatterNatural
        . normalize
        -- Convert to double _before_ normalizing. We may lose some precision
        -- here, but it is better than normalizing a natural, which will
        -- truncate (i.e. greater precision loss).
        . fmap (toDouble . unsafeConvertIntegral)

    toDouble :: Integer -> Double
    toDouble = fromℤ
{-# INLINEABLE handleLogFileSize #-}

-- | Ensures the given path exists. If the path already exists and the file
-- mode is FileModeRename, we rename the new path sequentially, to avoid
-- a collision.
createLogFile ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Mode in which to open the new log file.
  FileMode ->
  -- | Full path of the desired file.
  OsPath ->
  m OsPath
createLogFile mode fp = do
  exists <- doesFileExist fp

  if exists
    then case mode of
      FileModeRename -> do
        newFp <- uniqName fp
        writeFileUtf8 newFp "" $> newFp
      _ -> pure fp
    else writeFileUtf8 fp "" $> fp
{-# INLINEABLE createLogFile #-}

uniqName ::
  forall m.
  ( HasCallStack,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m OsPath
uniqName fp = go 1
  where
    (base, ext) = OsPath.splitExtension fp

    appendNum c = base <> [osp| (|] <> c <> [osp|)|] <> ext

    go :: Word16 -> m OsPath
    go !counter
      | counter == maxBound = die $ "Failed renaming file: " <> show fp
      | otherwise = do
          newFp <- appendNum <$> encodeThrowM (show counter)
          b <- doesFileExist newFp
          if b
            then go (counter + 1)
            else pure newFp

getShrunXdgState :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgState = getXdgState [osp|shrun|]
{-# INLINEABLE getShrunXdgState #-}

defaultToml :: FileLoggingToml
defaultToml =
  MkFileLoggingP
    { file =
        MkFileLogInitP
          { path = Nothing,
            mode = Nothing,
            sizeMode = Nothing
          },
      commandNameTrunc = Nothing,
      deleteOnSuccess = Nothing,
      lineTrunc = Nothing,
      stripControl = Nothing
    }
