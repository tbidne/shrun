{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging
  ( -- * Types
    ReportReadErrorsSwitch (..),
    CommandLoggingP (..),
    CommandLoggingArgs,
    CommandLoggingToml,
    CommandLoggingMerged,
    CommandLoggingEnv,

    -- * Functions
    mergeCommandLogging,
    toEnv,

    -- * Misc
    defaultMerged,
  )
where

import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    SwitchF,
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?), (<>?.))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

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
  { -- | How often to poll commands for logs, in microseconds.
    pollInterval :: ConfigPhaseF p PollInterval,
    -- | Determines the max log size we read from commands in one go.
    -- Note this is not on commandLogging or fileLogging since it affects both.
    readSize :: ConfigPhaseF p ReadSize,
    -- | Determines if we should log read errors.
    reportReadErrors :: SwitchF p ReportReadErrorsSwitch
  }

instance
  (k ~ A_Lens, a ~ ConfigPhaseF p PollInterval, b ~ ConfigPhaseF p PollInterval) =>
  LabelOptic "pollInterval" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _pollInterval
             _readSize
             _reportReadErrors
           ) ->
          fmap
            ( \pollInterval' ->
                MkCommandLoggingP
                  pollInterval'
                  _readSize
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
             _pollInterval
             _readSize
             _reportReadErrors
           ) ->
          fmap
            ( \readSize' ->
                MkCommandLoggingP
                  _pollInterval
                  readSize'
                  _reportReadErrors
            )
            (f _readSize)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ SwitchF p ReportReadErrorsSwitch, b ~ SwitchF p ReportReadErrorsSwitch) =>
  LabelOptic "reportReadErrors" k (CommandLoggingP p) (CommandLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkCommandLoggingP
             _pollInterval
             _readSize
             _reportReadErrors
           ) ->
          fmap
            ( MkCommandLoggingP
                _pollInterval
                _readSize
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
  ( Default (ConfigPhaseF p PollInterval),
    Default (ConfigPhaseF p ReadSize),
    Default (SwitchF p ReportReadErrorsSwitch)
  ) =>
  Default (CommandLoggingP p)
  where
  def =
    MkCommandLoggingP
      { pollInterval = def,
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
    { pollInterval =
        (args ^. #pollInterval) <>?. (toml ^. #pollInterval),
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

    toml = fromMaybe defaultToml mToml

instance DecodeTOML CommandLoggingToml where
  tomlDecoder =
    MkCommandLoggingP
      <$> decodePollInterval
      <*> decodeReadSize
      <*> decodeReportReadErrors

decodePollInterval :: Decoder (Maybe PollInterval)
decodePollInterval = getFieldOptWith tomlDecoder "poll-interval"

decodeReadSize :: Decoder (Maybe ReadSize)
decodeReadSize = getFieldOptWith tomlDecoder "read-size"

decodeReportReadErrors :: Decoder (Maybe Bool)
decodeReportReadErrors = getFieldOptWith tomlDecoder "report-read-errors"

-- | Creates env version from merged.
toEnv :: CommandLoggingMerged -> CommandLoggingEnv
toEnv merged =
  MkCommandLoggingP
    { pollInterval = merged ^. #pollInterval,
      readSize = merged ^. #readSize,
      reportReadErrors = merged ^. #reportReadErrors
    }

defaultToml :: CommandLoggingToml
defaultToml =
  MkCommandLoggingP
    { pollInterval = Nothing,
      readSize = Nothing,
      reportReadErrors = Nothing
    }

defaultMerged :: CommandLoggingMerged
defaultMerged =
  MkCommandLoggingP
    { pollInterval = def,
      readSize = def,
      reportReadErrors = def
    }
