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
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat)
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.Truncation
  ( TruncRegion (TruncCommandName),
    Truncation,
    configToLineTrunc,
    decodeCommandNameTrunc,
    decodeLineTrunc,
  )
import Shrun.Configuration.Data.WithDisabled ((<|?|>))
import Shrun.Configuration.Default (Default (def), (<.>))
import Shrun.Prelude

-- | Switch for command logging in console logs.
newtype ConsoleLogCmdSwitch = MkConsoleLogCmdSwitch Bool
  deriving stock (Eq, Show)
  deriving newtype (Bounded, Enum)

instance Default ConsoleLogCmdSwitch where
  def = MkConsoleLogCmdSwitch False

instance
  (k ~ An_Iso, a ~ Bool, b ~ Bool) =>
  LabelOptic
    "unConsoleLogCmdSwitch"
    k
    ConsoleLogCmdSwitch
    ConsoleLogCmdSwitch
    a
    b
  where
  labelOptic = iso (\(MkConsoleLogCmdSwitch b) -> b) MkConsoleLogCmdSwitch

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

instance
  ( k ~ A_Lens,
    a ~ SwitchF p ConsoleLogCmdSwitch,
    b ~ SwitchF p ConsoleLogCmdSwitch
  ) =>
  LabelOptic "commandLogging" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP b a2 a3 a4 a5)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName),
    b ~ ConfigPhaseDisabledMaybeF p (Truncation TruncCommandName)
  ) =>
  LabelOptic "commandNameTrunc" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 b a3 a4 a5)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LineTruncF p,
    b ~ LineTruncF p
  ) =>
  LabelOptic "lineTrunc" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 b a4 a5)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p ConsoleLogStripControl,
    b ~ ConfigPhaseF p ConsoleLogStripControl
  ) =>
  LabelOptic "stripControl" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 a3 b a5)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p TimerFormat,
    b ~ ConfigPhaseF p TimerFormat
  ) =>
  LabelOptic "timerFormat" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 a3 a4 b)
          (f a5)
  {-# INLINE labelOptic #-}

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
    MonadTerminal m
  ) =>
  ConsoleLoggingArgs ->
  Maybe ConsoleLoggingToml ->
  m ConsoleLoggingMerged
mergeConsoleLogging args mToml = do
  lineTrunc <-
    configToLineTrunc $ (args ^. #lineTrunc) <|?|> (toml ^. #lineTrunc)

  pure
    $ MkConsoleLoggingP
      { commandLogging =
          args
            ^. #commandLogging
            <.> (toml ^. #commandLogging),
        commandNameTrunc = (args ^. #commandNameTrunc) <|?|> (toml ^. #commandNameTrunc),
        lineTrunc,
        stripControl =
          (args ^. #stripControl) <.> (toml ^. #stripControl),
        timerFormat =
          (args ^. #timerFormat) <.> (toml ^. #timerFormat)
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
decodeCommandLogging =
  fmap MkConsoleLogCmdSwitch <$> getFieldOptWith tomlDecoder "command"

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
