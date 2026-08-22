{-# LANGUAGE TemplateHaskell #-}
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
import Shrun.Configuration.Data.CommandLogging.ReadStrategy qualified as RS
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    SwitchF,
    parseSwitch,
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Data.Graph (CommandGraph)
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Prelude

declareFieldLabels
  [d|
    newtype BufferLength = MkBufferLength {unBufferLength :: Int}
      deriving stock (Eq, Show)
      deriving (Num, Pretty) via Int
    |]

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

declareFieldLabels
  [d|
    newtype BufferTimeout = MkBufferTimeout {unBufferTimeout :: Timeout}
      deriving stock (Eq, Show)
      deriving newtype (FromInteger, Pretty)
    |]

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

declareFieldLabels
  [d|
    -- Switch for logging read errors
    newtype ReportReadErrorsSwitch = MkReportReadErrorsSwitch {unReportReadErrorsSwitch :: Bool}
      deriving stock (Eq, Show)
      deriving newtype (Bounded, Enum)
    |]

instance Default ReportReadErrorsSwitch where
  def = MkReportReadErrorsSwitch False

instance DecodeTOML ReportReadErrorsSwitch where
  tomlDecoder = MkReportReadErrorsSwitch <$> (tomlDecoder >>= parseSwitch)

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
    readStrategy :: ConfigPhaseF p ReadStrategy,
    -- | Determines if we should log read errors.
    reportReadErrors :: SwitchF p ReportReadErrorsSwitch
  }

makeFieldLabelsNoPrefix ''CommandLoggingP

instance Semigroup CommandLoggingToml where
  l <> r =
    MkCommandLoggingP
      { bufferLength = l ^. #bufferLength <|> r ^. #bufferLength,
        bufferTimeout = l ^. #bufferTimeout <|> r ^. #bufferTimeout,
        pollInterval = l ^. #pollInterval <|> r ^. #pollInterval,
        readSize = l ^. #readSize <|> r ^. #readSize,
        readStrategy = l ^. #readStrategy <|> r ^. #readStrategy,
        reportReadErrors = l ^. #reportReadErrors <|> r ^. #reportReadErrors
      }

instance Monoid CommandLoggingToml where
  mempty =
    MkCommandLoggingP
      { bufferLength = Nothing,
        bufferTimeout = Nothing,
        pollInterval = Nothing,
        readSize = Nothing,
        readStrategy = Nothing,
        reportReadErrors = Nothing
      }

instance Pretty CommandLoggingMerged where
  pretty c =
    vcat
      [ "buffer-length: " <> pretty (c ^. #bufferLength),
        "buffer-timeout: " <> pretty (c ^. #bufferTimeout),
        "poll-interval: " <> pretty (c ^. #pollInterval),
        "read-size: " <> pretty (c ^. #readSize),
        "read-strategy: " <> pretty (c ^. #readStrategy)
        -- reportReadErrors intentionally unexposed
      ]

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

instance Default CommandLoggingArgs where
  def =
    MkCommandLoggingP
      { bufferLength = Nothing,
        bufferTimeout = Nothing,
        pollInterval = Nothing,
        readStrategy = Nothing,
        readSize = Nothing,
        reportReadErrors = Nothing
      }

-- | Merges args and toml configs.
mergeCommandLogging ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  Bool ->
  Bool ->
  CommandGraph ->
  CommandLoggingArgs ->
  Maybe CommandLoggingToml ->
  m CommandLoggingMerged
mergeCommandLogging isFileLog isFileLogMulti cmdGraph args mToml = do
  readStrategy <-
    guardReadStrategy
      ((args ^. #readStrategy) <|> (toml ^. #readStrategy))

  pure
    $ MkCommandLoggingP
      { bufferLength =
          (args ^. #bufferLength) <.> (toml ^. #bufferLength),
        bufferTimeout =
          (args ^. #bufferTimeout) <.> (toml ^. #bufferTimeout),
        pollInterval =
          (args ^. #pollInterval) <.> (toml ^. #pollInterval),
        readStrategy,
        readSize =
          (args ^. #readSize) <.> (toml ^. #readSize),
        reportReadErrors =
          args ^. #reportReadErrors <.> (toml ^. #reportReadErrors)
      }
  where
    toml = fromMaybe mempty mToml

    -- In general we want to let the user pick or pick a good default, but
    -- we need to verify ReadBlockLineBuffer strategy is okay if the user
    -- selects it.
    guardReadStrategy = \case
      -- 1. User set ReadBlockLineBuffer, verify it's okay.
      Just ReadBlockLineBuffer ->
        if RS.readBlockLineBufferNotAllowed isFileLog isFileLogMulti cmdGraph
          then throwM MkReadStrategyException
          else pure ReadBlockLineBuffer
      -- 2. User set ReadBlock, fine.
      Just ReadBlock -> pure ReadBlock
      -- 3. User did not specify. Pick a good default.
      Nothing -> pure $ RS.defaultReadStrategy isFileLog isFileLogMulti cmdGraph

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

decodeReportReadErrors :: Decoder (Maybe ReportReadErrorsSwitch)
decodeReportReadErrors = getFieldOptWith tomlDecoder "report-read-errors"

-- | Creates env version from merged. Requires commands because we pick
-- the read strategy based on the number of commands.
toEnv :: CommandLoggingMerged -> CommandLoggingEnv
toEnv merged =
  MkCommandLoggingP
    { bufferLength = merged ^. #bufferLength,
      bufferTimeout = merged ^. #bufferTimeout,
      pollInterval = merged ^. #pollInterval,
      readStrategy = merged ^. #readStrategy,
      readSize = merged ^. #readSize,
      reportReadErrors = merged ^. #reportReadErrors
    }

data ReadStrategyException = MkReadStrategyException
  deriving stock (Eq, Show)

instance Exception ReadStrategyException where
  displayException _ =
    mconcat
      [ "The --command-log-read-strategy 'block-line-buffer' strategy was ",
        "specified, however, it is invalid when there are multiple commands ",
        "and file-logging is enabled."
      ]
