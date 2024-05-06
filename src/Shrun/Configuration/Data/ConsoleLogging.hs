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

    -- * Misc
    defaultMerged,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    LineTruncF,
    SwitchF,
  )
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.Truncation
  ( TruncRegion (TCmdName),
    Truncation,
    configToLineTrunc,
    decodeCmdNameTrunc,
    decodeLineTrunc,
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<>?), (<>?.), (<>??))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Switch for command logging in console logs.
data ConsoleLogCmdSwitch
  = ConsoleLogCmdOff
  | ConsoleLogCmdOn
  deriving stock (Eq, Show)

instance Default ConsoleLogCmdSwitch where
  def = ConsoleLogCmdOff

instance
  ( k ~ An_Iso,
    a ~ Bool,
    b ~ Bool
  ) =>
  LabelOptic
    "boolIso"
    k
    ConsoleLogCmdSwitch
    ConsoleLogCmdSwitch
    a
    b
  where
  labelOptic =
    iso
      (\cases ConsoleLogCmdOn -> True; ConsoleLogCmdOff -> False)
      (\cases True -> ConsoleLogCmdOn; False -> ConsoleLogCmdOff)

-- | Holds command logging config.
type ConsoleLoggingP :: ConfigPhase -> Type
data ConsoleLoggingP p = MkConsoleLoggingP
  { cmdLogging :: SwitchF p ConsoleLogCmdSwitch,
    cmdNameTrunc :: ConfigPhaseMaybeF p (Truncation TCmdName),
    lineTrunc :: LineTruncF p,
    stripControl :: ConfigPhaseF p ConsoleLogStripControl
  }

makeFieldLabelsNoPrefix ''ConsoleLoggingP

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

-- | Merges args and toml configs.
mergeConsoleLogging ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  ConsoleLoggingArgs ->
  Maybe ConsoleLoggingToml ->
  m ConsoleLoggingMerged
mergeConsoleLogging args mToml = do
  lineTrunc <-
    configToLineTrunc $ (args ^. #lineTrunc) <>? (toml ^. #lineTrunc)

  pure
    $ MkConsoleLoggingP
      { cmdLogging =
          WD.fromDefault
            ( review #boolIso
                <$> argsCmdLogging
                <>? (toml ^. #cmdLogging)
            ),
        cmdNameTrunc = (args ^. #cmdNameTrunc) <>?? (toml ^. #cmdNameTrunc),
        lineTrunc,
        stripControl =
          (args ^. #stripControl) <>?. (toml ^. #stripControl)
      }
  where
    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    argsCmdLogging :: WithDisabled Bool
    argsCmdLogging = args ^. #cmdLogging $> True

    toml = fromMaybe defaultToml mToml

instance DecodeTOML ConsoleLoggingToml where
  tomlDecoder =
    MkConsoleLoggingP
      <$> decodeCmdLogging
      <*> decodeCmdNameTrunc
      <*> decodeLineTrunc
      <*> decodeStripControl

decodeCmdLogging :: Decoder (Maybe Bool)
decodeCmdLogging = getFieldOptWith tomlDecoder "cmd"

decodeStripControl :: Decoder (Maybe ConsoleLogStripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

toEnv :: ConsoleLoggingMerged -> ConsoleLoggingEnv
toEnv merged =
  MkConsoleLoggingP
    { cmdLogging = merged ^. #cmdLogging,
      cmdNameTrunc = merged ^. #cmdNameTrunc,
      lineTrunc = merged ^. #lineTrunc,
      stripControl = merged ^. #stripControl
    }

defaultToml :: ConsoleLoggingToml
defaultToml =
  MkConsoleLoggingP
    { cmdLogging = Nothing,
      cmdNameTrunc = Nothing,
      lineTrunc = Nothing,
      stripControl = Nothing
    }

defaultMerged :: ConsoleLoggingMerged
defaultMerged =
  MkConsoleLoggingP
    { cmdLogging = def,
      cmdNameTrunc = Nothing,
      lineTrunc = Nothing,
      stripControl = def
    }
