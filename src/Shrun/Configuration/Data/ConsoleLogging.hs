{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.ConsoleLogging
  ( -- * Types
    ConsoleLoggingP (..),
    ConsoleLoggingArgs,
    ConsoleLoggingToml,
    ConsoleLoggingMerged,
    ConsoleLoggingEnv,
    ConsoleLogCmdSwitch (..),

    -- * Functions
    mergeConsoleLogging,
    toEnv,
  )
where

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
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat)
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.Truncation
  ( DetectResult,
    TruncRegion (TruncCommandName),
    Truncation,
    decodeCommandNameTrunc,
    decodeLineTrunc,
    mergeLineTrunc,
  )
import Shrun.Configuration.Data.WithDisabled ((<|?|>))
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Prelude

declareFieldLabels
  [d|
    -- Switch for command logging in console logs.
    newtype ConsoleLogCmdSwitch = MkConsoleLogCmdSwitch {unConsoleLogCmdSwitch :: Bool}
      deriving stock (Eq, Show)
      deriving newtype (Bounded, Enum)
      deriving (Pretty) via PrettySwitch
    |]

instance Default ConsoleLogCmdSwitch where
  def = MkConsoleLogCmdSwitch True

instance DecodeTOML ConsoleLogCmdSwitch where
  tomlDecoder = MkConsoleLogCmdSwitch <$> (tomlDecoder >>= parseSwitch)

-- | Holds command logging config.
type ConsoleLoggingP :: ConfigPhase -> Type
data ConsoleLoggingP p = MkConsoleLoggingP
  { -- | Whether command logging is With.
    commandLogging :: SwitchF p ConsoleLogCmdSwitch,
    -- | Command name truncation.
    commandNameTrunc :: ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName),
    -- | Line truncation.
    lineTrunc :: LineTruncF p,
    -- | Strip control.
    stripControl :: ConfigPhaseF p ConsoleLogStripControl,
    -- | How to format the timer.
    timerFormat :: ConfigPhaseF p TimerFormat
  }

makeFieldLabelsNoPrefix ''ConsoleLoggingP

instance Semigroup ConsoleLoggingToml where
  l <> r =
    MkConsoleLoggingP
      { commandLogging = l ^. #commandLogging <|> r ^. #commandLogging,
        commandNameTrunc = l ^. #commandNameTrunc <|> r ^. #commandNameTrunc,
        lineTrunc = l ^. #lineTrunc <|> r ^. #lineTrunc,
        stripControl = l ^. #stripControl <|> r ^. #stripControl,
        timerFormat = l ^. #timerFormat <|> r ^. #timerFormat
      }

instance Monoid ConsoleLoggingToml where
  mempty =
    MkConsoleLoggingP
      { commandLogging = Nothing,
        commandNameTrunc = Nothing,
        lineTrunc = Nothing,
        stripControl = Nothing,
        timerFormat = Nothing
      }

instance Pretty ConsoleLoggingMerged where
  pretty c =
    vcat
      [ "command-logging: " <> pretty (c ^. #commandLogging),
        "command-name-trunc: " <> prettyMaybe (c ^. #commandNameTrunc),
        "line-trunc: " <> prettyMaybe (c ^. #lineTrunc),
        "strip-control: " <> pretty (c ^. #stripControl),
        "timer-format: " <> pretty (c ^. #timerFormat)
      ]

type ConsoleLoggingArgs = ConsoleLoggingP ConfigPhaseArgs

type ConsoleLoggingToml = ConsoleLoggingP ConfigPhaseToml

type ConsoleLoggingMerged = ConsoleLoggingP ConfigPhaseMerged

type ConsoleLoggingEnv = ConsoleLoggingP ConfigPhaseEnv

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseMerged)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseMerged)

instance Default ConsoleLoggingArgs where
  def =
    MkConsoleLoggingP
      { commandLogging = Nothing,
        commandNameTrunc = Nothing,
        lineTrunc = Nothing,
        stripControl = Nothing,
        timerFormat = Nothing
      }

-- | Merges args and toml configs.
mergeConsoleLogging ::
  ( HasCallStack,
    MonadCatch m,
    MonadIORef m,
    MonadTerminal m
  ) =>
  IORef DetectResult ->
  ConsoleLoggingArgs ->
  Maybe ConsoleLoggingToml ->
  m ConsoleLoggingMerged
mergeConsoleLogging detectRef args mToml = do
  let commandLogging =
        args
          ^. #commandLogging
          <.> toml
          ^. #commandLogging
      -- Default to 'detect' iff command logging is on. We do this because the
      -- only logs likely to need line truncation are long command  logs.
      defDetect = commandLogging ^. #unConsoleLogCmdSwitch

  lineTrunc <-
    mergeLineTrunc defDetect detectRef (args ^. #lineTrunc) (toml ^. #lineTrunc)

  pure
    $ MkConsoleLoggingP
      { commandLogging,
        commandNameTrunc = args ^. #commandNameTrunc <|?|> toml ^. #commandNameTrunc,
        lineTrunc,
        stripControl = args ^. #stripControl <.> toml ^. #stripControl,
        timerFormat = args ^. #timerFormat <.> toml ^. #timerFormat
      }
  where
    toml = fromMaybe mempty mToml
{-# INLINEABLE mergeConsoleLogging #-}

instance DecodeTOML ConsoleLoggingToml where
  tomlDecoder =
    MkConsoleLoggingP
      <$> decodeCommandLogging
      <*> decodeCommandNameTrunc
      <*> decodeLineTrunc
      <*> decodeStripControl
      <*> decodeTimerFormat

decodeCommandLogging :: Decoder (Maybe ConsoleLogCmdSwitch)
decodeCommandLogging = getFieldOptWith tomlDecoder "command"

decodeStripControl :: Decoder (Maybe ConsoleLogStripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeTimerFormat :: Decoder (Maybe TimerFormat)
decodeTimerFormat = getFieldOptWith tomlDecoder "timer-format"

toEnv :: ConsoleLoggingMerged -> ConsoleLoggingEnv
toEnv merged =
  MkConsoleLoggingP
    { commandLogging = merged ^. #commandLogging,
      commandNameTrunc = merged ^. #commandNameTrunc,
      lineTrunc = merged ^. #lineTrunc,
      stripControl = merged ^. #stripControl,
      timerFormat = merged ^. #timerFormat
    }
