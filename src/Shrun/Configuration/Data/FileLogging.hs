{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.FileLogging
  ( FileLogInitP (..),
    FileLogOpened (..),
    FileLoggingP (..),
    FileLoggingArgs,
    FileLoggingToml,
    FileLoggingMerged,
    FileLoggingEnv,
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
import Effects.FileSystem.HandleWriter (MonadHandleWriter (withBinaryFile))
import Effects.FileSystem.PathWriter (MonadPathWriter (createDirectoryIfMissing))
import Effects.FileSystem.Utils qualified as FsUtils
import GHC.Num (Num (fromInteger))
import Shrun.Configuration.Data.ConfigPhase
  ( BoolF,
    ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    LineTruncF,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<>?),
    _With,
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Data.FileMode (FileMode (FileModeAppend, FileModeWrite), defaultFileMode)
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.FileSizeMode (FileSizeMode (..), defaultFileSizeMode)
import Shrun.Data.StripControl (StripControl, defaultFileLogStripControl)
import Shrun.Data.Truncation (TruncRegion (TCmdName), Truncation, configToLineTrunc, decodeCmdNameTrunc, decodeLineTrunc)
import Shrun.Logging.Types (FileLog)
import Shrun.Prelude

-- NOTE: [Args vs. Toml mandatory fields]
--
-- Some fields are mandatory e.g. FileLogging's path if we are actually
-- doing file logging. The latter is determined by the FileLoggingP itself
-- being Just (cf. Nothing), thus the path itself is mandatory on Toml and
-- Merged.
--
-- So why is it optional on Args? Because Args' FileLoggingP is _always_
-- present, unlike Toml and Merged's Maybe. We need this behavior because the
-- former's fields can be used to override toml fields, even if file-logging is
-- not specified on the CLI.
--
-- For example, 'shrun --file-log-mode write cmd' _should_ overwrite toml's
-- file-log.mode even though we did not specify --file-log. Therefore Args'
-- FileLoggingP always needs to be present hence all its field must be
-- optional, even when some are mandatory on Merged.

-- | File logging's path is only optional for the Args. For Toml and merged,
-- it must be present if file logging is active.
type FileLogPathF :: ConfigPhase -> Type
type family FileLogPathF p where
  FileLogPathF ConfigPhaseArgs = WithDisabled FilePathDefault
  FileLogPathF ConfigPhaseToml = FilePathDefault
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

makeFieldLabelsNoPrefix ''FileLogInitP

type FileLogInitArgs = FileLogInitP ConfigPhaseArgs

type FileLogInitToml = FileLogInitP ConfigPhaseToml

type FileLogInitMerged = FileLogInitP ConfigPhaseMerged

deriving stock instance Eq FileLogInitArgs

deriving stock instance Show FileLogInitArgs

deriving stock instance Eq FileLogInitToml

deriving stock instance Show FileLogInitToml

deriving stock instance Eq FileLogInitMerged

deriving stock instance Show FileLogInitMerged

instance DecodeTOML FileLogInitToml where
  tomlDecoder =
    MkFileLogInitP
      <$> decodeFileLogging
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

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

makeFieldLabelsNoPrefix ''FileLogOpened

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
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    -- | If active, deletes the log file upon success.
    deleteOnSuccess :: BoolF p,
    -- | Determines to what extent we should remove control characters
    -- from file logs.
    lineTrunc :: LineTruncF p,
    stripControl :: ConfigPhaseF p StripControl
  }

makeFieldLabelsNoPrefix ''FileLoggingP

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

-- | Merges args and toml configs.
mergeFileLogging ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  FileLoggingArgs ->
  Maybe FileLoggingToml ->
  m (Maybe FileLoggingMerged)
mergeFileLogging args mToml =
  case args ^. (#file % #path) of
    -- 1. Logging globally disabled
    Disabled -> pure Nothing
    Without -> case mToml of
      -- 2. No Args and no Toml
      Nothing -> pure Nothing
      -- 3. No Args and yes Toml
      Just toml -> do
        lineTrunc <-
          configToLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml
        pure
          $ Just
          $ MkFileLoggingP
            { file =
                MkFileLogInitP
                  { path = toml ^. (#file % #path),
                    mode =
                      plusDefault
                        defaultFileMode
                        (#file % #mode)
                        (toml ^. #file % #mode),
                    sizeMode =
                      plusDefault
                        defaultFileSizeMode
                        (#file % #sizeMode)
                        (toml ^. #file % #sizeMode)
                  },
              cmdNameTrunc =
                plusNothing
                  #cmdNameTrunc
                  (toml ^. #cmdNameTrunc),
              deleteOnSuccess =
                WD.fromWithDisabled
                  False
                  (argsDeleteOnSuccess <>? (toml ^. #deleteOnSuccess)),
              lineTrunc,
              stripControl =
                plusDefault
                  defaultFileLogStripControl
                  #stripControl
                  (toml ^. #stripControl)
            }
    With path -> case mToml of
      -- 3. Yes Args and no Toml
      Nothing -> do
        lineTrunc <- configToLineTrunc (args ^. #lineTrunc)
        pure
          $ Just
          $ MkFileLoggingP
            { file =
                MkFileLogInitP
                  { path,
                    mode =
                      WD.fromWithDisabled defaultFileMode (args ^. #file % #mode),
                    sizeMode =
                      WD.fromWithDisabled defaultFileSizeMode (args ^. #file % #sizeMode)
                  },
              cmdNameTrunc =
                WD.toMaybe (args ^. #cmdNameTrunc),
              deleteOnSuccess = is (#deleteOnSuccess % _With) args,
              lineTrunc,
              stripControl =
                WD.fromWithDisabled
                  defaultFileLogStripControl
                  (args ^. #stripControl)
            }
      -- 4. Yes Args and yes Toml
      Just toml -> do
        lineTrunc <-
          configToLineTrunc $ view #lineTrunc args <>? view #lineTrunc toml
        pure
          $ Just
          $ MkFileLoggingP
            { file =
                MkFileLogInitP
                  { path,
                    mode =
                      plusDefault
                        defaultFileMode
                        (#file % #mode)
                        (toml ^. #file % #mode),
                    sizeMode =
                      plusDefault
                        defaultFileSizeMode
                        (#file % #sizeMode)
                        (toml ^. #file % #sizeMode)
                  },
              cmdNameTrunc =
                plusNothing
                  #cmdNameTrunc
                  (toml ^. #cmdNameTrunc),
              deleteOnSuccess =
                WD.fromWithDisabled
                  False
                  (argsDeleteOnSuccess <>? (toml ^. #deleteOnSuccess)),
              lineTrunc,
              stripControl =
                plusDefault
                  defaultFileLogStripControl
                  #stripControl
                  (view #stripControl toml)
            }
  where
    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    argsDeleteOnSuccess :: WithDisabled Bool
    argsDeleteOnSuccess = args ^. #deleteOnSuccess $> True

    plusDefault :: a -> Lens' FileLoggingArgs (WithDisabled a) -> Maybe a -> a
    plusDefault defA l r = WD.fromWithDisabled defA $ (args ^. l) <>? r

    plusNothing :: Lens' FileLoggingArgs (WithDisabled a) -> Maybe a -> Maybe a
    plusNothing l r = WD.toMaybe $ (args ^. l) <>? r

instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingP
      <$> tomlDecoder
      <*> decodeCmdNameTrunc
      <*> decodeFileDeleteOnSuccess
      <*> decodeLineTrunc
      <*> decodeFileLogStripControl

decodeFileDeleteOnSuccess :: Decoder (Maybe Bool)
decodeFileDeleteOnSuccess = getFieldOptWith tomlDecoder "delete-on-success"

decodeFileLogStripControl :: Decoder (Maybe StripControl)
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
    MonadTerminal m
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
              cmdNameTrunc = fl ^. #cmdNameTrunc,
              lineTrunc = fl ^. #lineTrunc,
              deleteOnSuccess = fl ^. #deleteOnSuccess,
              stripControl = fl ^. #stripControl
            }

  withMLogging mFileLogging (onFileLoggingEnv . mkEnv)

withMLogging ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  Maybe FileLoggingMerged ->
  (MLogging -> m a) ->
  m a
-- 1. No file logging
withMLogging Nothing onLogging = onLogging Nothing
-- 2. Use the default path.
withMLogging (Just fileLogging) onLogging = do
  let ioMode = case fileLogging ^. #file % #mode of
        FileModeAppend -> AppendMode
        FileModeWrite -> WriteMode

  fp <- case fileLogging ^. #file % #path of
    FPDefault -> do
      stateDir <- getShrunXdgState
      let fp = stateDir </> [osp|shrun.log|]
      stateExists <- doesDirectoryExist stateDir
      unless stateExists (createDirectoryIfMissing True stateDir)
      pure fp
    FPManual fp -> pure fp

  ensureFileExists fp
  handleLogFileSize (fileLogging ^. #file % #sizeMode) fp
  fileQueue <- newTBQueueA 1000

  result <-
    withBinaryFile fp ioMode $ \h ->
      onLogging (Just (fileLogging, h, fileQueue))

  -- If the above command succeeded and deleteOnSuccess is true, delete the
  -- log file. Otherwise we will not reach here due to withBinaryFile
  -- rethrowing an exception, so the file will not be deleted.
  when (fileLogging ^. #deleteOnSuccess)
    $ removeFileIfExists fp

  pure result

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
          T.pack $ FsUtils.decodeOsToFpShow fp,
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
    toDouble = fromInteger

ensureFileExists ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m
  ) =>
  OsPath ->
  m ()
ensureFileExists fp = do
  exists <- doesFileExist fp
  unless exists $ writeFileUtf8 fp ""

getShrunXdgState :: (HasCallStack, MonadPathReader m) => m OsPath
getShrunXdgState = getXdgState [osp|shrun|]
