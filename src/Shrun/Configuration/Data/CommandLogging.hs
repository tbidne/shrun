{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging
  ( -- * Types

    -- ** Buffer length
    BufferLength (..),
    parseBufferLength,

    -- ** Buffer timeout
    BufferTimeout (..),
    parseBufferTimeout,

    -- ** Report read errors
    ReportReadErrorsSwitch (..),

    -- ** Main
    CommandLoggingP (..),
    CommandLoggingArgs,
    CommandLoggingToml,
    CommandLoggingMerged,
    CommandLoggingEnv,

    -- * Functions
    mergeCommandLogging,
    toEnv,

    -- * Exceptions
    ReadStrategyException (..),
  )
where

import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize)
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock,
        ReadBlockLineBuffer
      ),
  )
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseEnvF,
    ConfigPhaseF,
    SwitchF,
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?), (<>?.), (<>??))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default (def))
import Shrun.Data.Command (CommandP1)
import Shrun.Prelude

newtype BufferLength = MkBufferLength Int
  deriving stock (Eq, Show)
  deriving (Num) via Int

instance
  (k ~ An_Iso, a ~ Int, b ~ Int) =>
  LabelOptic "unBufferLength" k BufferLength BufferLength a b
  where
  labelOptic = iso (\(MkBufferLength x) -> x) MkBufferLength
  {-# INLINE labelOptic #-}

instance Default BufferLength where
  def = MkBufferLength 1_000

instance DecodeTOML BufferLength where
  tomlDecoder = MkBufferLength <$> tomlDecoder

parseBufferLength :: (MonadFail m) => m Natural -> m BufferLength
parseBufferLength getNat = do
  n <- getNat
  case convertIntegral n of
    Left err -> fail err
    Right x -> pure $ MkBufferLength x
{-# INLINEABLE parseBufferLength #-}

newtype BufferTimeout = MkBufferTimeout Timeout
  deriving stock (Eq, Show)
  deriving (Num) via Natural

instance
  (k ~ An_Iso, a ~ Timeout, b ~ Timeout) =>
  LabelOptic "unBufferTimeout" k BufferTimeout BufferTimeout a b
  where
  labelOptic = iso (\(MkBufferTimeout x) -> x) MkBufferTimeout
  {-# INLINE labelOptic #-}

instance Default BufferTimeout where
  def = MkBufferTimeout 30

instance DecodeTOML BufferTimeout where
  tomlDecoder = MkBufferTimeout <$> tomlDecoder

parseBufferTimeout ::
  (Alternative f, MonadFail f) =>
  f Natural ->
  f Text ->
  f BufferTimeout
parseBufferTimeout getNat getTxt =
  MkBufferTimeout <$> Timeout.parseTimeout getNat getTxt
{-# INLINEABLE parseBufferTimeout #-}

-- | Switch for logging read errors
data ReportReadErrorsSwitch
  = ReportReadErrorsOff
  | ReportReadErrorsOn
  deriving stock (Eq, Show)

instance Default ReportReadErrorsSwitch where
  def = ReportReadErrorsOff

instance
  ( k ~ An_Iso,
    a ~ Bool,
    b ~ Bool
  ) =>
  LabelOptic
    "boolIso"
    k
    ReportReadErrorsSwitch
    ReportReadErrorsSwitch
    a
    b
  where
  labelOptic =
    iso
      (\cases ReportReadErrorsOn -> True; ReportReadErrorsOff -> False)
      (\cases True -> ReportReadErrorsOn; False -> ReportReadErrorsOff)
  {-# INLINE labelOptic #-}

-- | Holds config related to (console and file) command logging.
type CommandLoggingP :: ConfigPhase -> Type
data CommandLoggingP p = MkCommandLoggingP
  { -- | Max log length held by the buffer for the ReadBlockLineBuffer
    -- ReadStrategy.
    bufferLength :: ConfigPhaseF p BufferLength,
    -- | Max time the buffer will hold a log before flushing it, for the
    -- ReadBlockLineBuffer ReadStrategy.
    bufferTimeout :: ConfigPhaseF p BufferTimeout,
    -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on commandLogging or fileLogging since it affects both.
    readSize :: ConfigPhaseF p ReadSize,
    -- | Reading strategy.
    readStrategy :: ConfigPhaseEnvF p ReadStrategy,
    -- | Determines if we should log read errors.
    reportReadErrors :: SwitchF p ReportReadErrorsSwitch
  }

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p BufferLength, b ~ ConfigPhaseF p BufferLength) =>
  LabelOptic "bufferLength" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( \bufferLength' ->
                MkCommandLoggingP
                  bufferLength'
                  _bufferTimeout
                  _pollInterval
                  _readSize
                  _readStrategy
                  _reportReadErrors
            )
            (f _bufferLength)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p BufferTimeout, b ~ ConfigPhaseF p BufferTimeout) =>
  LabelOptic "bufferTimeout" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( \bufferTimeout' ->
                MkCommandLoggingP
                  _bufferLength
                  bufferTimeout'
                  _pollInterval
                  _readSize
                  _readStrategy
                  _reportReadErrors
            )
            (f _bufferTimeout)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p PollInterval, b ~ ConfigPhaseF p PollInterval) =>
  LabelOptic "pollInterval" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( \pollInterval' ->
                MkCommandLoggingP
                  _bufferLength
                  _bufferTimeout
                  pollInterval'
                  _readSize
                  _readStrategy
                  _reportReadErrors
            )
            (f _pollInterval)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p ReadSize, b ~ ConfigPhaseF p ReadSize) =>
  LabelOptic "readSize" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( \readSize' ->
                MkCommandLoggingP
                  _bufferLength
                  _bufferTimeout
                  _pollInterval
                  readSize'
                  _readStrategy
                  _reportReadErrors
            )
            (f _readSize)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ConfigPhaseEnvF p ReadStrategy, b ~ ConfigPhaseEnvF p ReadStrategy) =>
  LabelOptic "readStrategy" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( \readStrategy' ->
                MkCommandLoggingP
                  _bufferLength
                  _bufferTimeout
                  _pollInterval
                  _readSize
                  readStrategy'
                  _reportReadErrors
            )
            (f _readStrategy)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ SwitchF p ReportReadErrorsSwitch, b ~ SwitchF p ReportReadErrorsSwitch) =>
  LabelOptic "reportReadErrors" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _bufferLength
             _bufferTimeout
             _pollInterval
             _readSize
             _readStrategy
             _reportReadErrors
           ) ->
          fmap
            ( MkCommandLoggingP
                _bufferLength
                _bufferTimeout
                _pollInterval
                _readSize
                _readStrategy
            )
            (f _reportReadErrors)
  {-# INLINE labelOptic #-}

type CommandLoggingArgs = CommandLoggingP ConfigPhaseArgs

type CommandLoggingToml = CommandLoggingP ConfigPhaseToml

type CommandLoggingMerged = CommandLoggingP ConfigPhaseMerged

type CommandLoggingEnv = CommandLoggingP ConfigPhaseEnv

deriving stock instance Eq (CommandLoggingP ConfigPhaseArgs)

deriving stock instance Show (CommandLoggingP ConfigPhaseArgs)

deriving stock instance Eq (CommandLoggingP ConfigPhaseToml)

deriving stock instance Show (CommandLoggingP ConfigPhaseToml)

deriving stock instance Eq (CommandLoggingP ConfigPhaseMerged)

deriving stock instance Show (CommandLoggingP ConfigPhaseMerged)

instance
  ( Default (ConfigPhaseF p BufferLength),
    Default (ConfigPhaseF p BufferTimeout),
    Default (ConfigPhaseF p PollInterval),
    Default (ConfigPhaseEnvF p ReadStrategy),
    Default (ConfigPhaseF p ReadSize),
    Default (SwitchF p ReportReadErrorsSwitch)
  ) =>
  Default (CommandLoggingP p)
  where
  def =
    MkCommandLoggingP
      { bufferLength = def,
        bufferTimeout = def,
        pollInterval = def,
        readStrategy = def,
        readSize = def,
        reportReadErrors = def
      }

-- | Merges args and toml configs.
mergeCommandLogging ::
  CommandLoggingArgs ->
  Maybe CommandLoggingToml ->
  CommandLoggingMerged
mergeCommandLogging args mToml =
  MkCommandLoggingP
    { bufferLength =
        (args ^. #bufferLength) <>?. (toml ^. #bufferLength),
      bufferTimeout =
        (args ^. #bufferTimeout) <>?. (toml ^. #bufferTimeout),
      pollInterval =
        (args ^. #pollInterval) <>?. (toml ^. #pollInterval),
      readStrategy =
        (args ^. #readStrategy) <>?? (toml ^. #readStrategy),
      readSize =
        (args ^. #readSize) <>?. (toml ^. #readSize),
      reportReadErrors =
        WD.fromDefault
          ( review #boolIso
              <$> argsReportReadErrors
              <>? (toml ^. #reportReadErrors)
          )
    }
  where
    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    argsReportReadErrors :: WithDisabled Bool
    argsReportReadErrors = args ^. #reportReadErrors $> True

    toml = fromMaybe def mToml

instance DecodeTOML CommandLoggingToml where
  tomlDecoder =
    MkCommandLoggingP
      <$> decodeBufferLength
      <*> decodeBufferTimeout
      <*> decodePollInterval
      <*> decodeReadSize
      <*> decodeReadStrategy
      <*> decodeReportReadErrors

decodeBufferLength :: Decoder (Maybe BufferLength)
decodeBufferLength = getFieldOptWith tomlDecoder "buffer-length"

decodeBufferTimeout :: Decoder (Maybe BufferTimeout)
decodeBufferTimeout = getFieldOptWith tomlDecoder "buffer-timeout"

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeReadSize :: Decoder (Maybe ReadSize)
decodeReadSize = getFieldOptWith tomlDecoder "read-size"

decodeReadStrategy :: Decoder (Maybe ReadStrategy)
decodeReadStrategy = getFieldOptWith tomlDecoder "read-strategy"

decodeReportReadErrors :: Decoder (Maybe Bool)
decodeReportReadErrors = getFieldOptWith tomlDecoder "report-read-errors"

-- | Creates env version from merged. Requires commands because we pick
-- the read strategy based on the number of commands.
toEnv ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  NESeq CommandP1 ->
  CommandLoggingMerged ->
  m CommandLoggingEnv
toEnv cmds merged = do
  readStrategy <- mkReadStrategy
  pure
    $ MkCommandLoggingP
      { bufferLength = merged ^. #bufferLength,
        bufferTimeout = merged ^. #bufferTimeout,
        pollInterval = merged ^. #pollInterval,
        readStrategy,
        readSize = merged ^. #readSize,
        reportReadErrors = merged ^. #reportReadErrors
      }
  where
    -- In general we want to let the user pick or pick a good default, but
    -- we need to verify ReadBlockLineBuffer strategy is okay if the user
    -- selects it.
    mkReadStrategy = case merged ^. #readStrategy of
      -- 1. User set ReadBlockLineBuffer, verify it's okay.
      Just ReadBlockLineBuffer ->
        if readBlockLineBufferAllowed
          then pure ReadBlockLineBuffer
          else throwM MkReadStrategyException
      -- 2. User set ReadBlock, fine.
      Just ReadBlock -> pure ReadBlock
      -- 3. User did not specify. Pick a good default.
      Nothing ->
        if readBlockLineBufferAllowed
          then pure ReadBlockLineBuffer
          else pure ReadBlock

    readBlockLineBufferAllowed = length cmds == 1

data ReadStrategyException = MkReadStrategyException
  deriving stock (Eq, Show)

instance Exception ReadStrategyException where
  displayException _ =
    mconcat
      [ "The --command-log-read-strategy 'block-line-buffer' strategy was ",
        "specified, however, it is only valid when there is exactly one ",
        "command."
      ]
