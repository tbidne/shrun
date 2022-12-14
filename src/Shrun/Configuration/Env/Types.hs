{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
--
-- @since 0.5
module Shrun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasLogging (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    Logging (..),
    CmdLogging (..),
    FileLogging (..),
    CmdDisplay (..),
    Truncation (..),
    LineTruncation (..),
    TruncRegion (..),
    StripControl (..),

    -- * Optics
    _HideKey,
    _ShowKey,
    _TCmdName,
    _TCmdLine,
    _StripControlSmart,
    _StripControlAll,
    _StripControlNone,
    _Undetected,
    _Detected,
  )
where

import Data.Sequence (Seq)
import GHC.Show (appPrec, appPrec1)
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Supremum (Supremum (..))
import Shrun.Data.Timeout (Timeout)
import Shrun.Logging.Types (FileLog, LogRegion)
import Shrun.Prelude
import TOML (Value (Integer, String))
import Text.Show (showParen, showString)

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
--
-- @since 0.1
data CmdDisplay
  = -- | Display the command's key, if it exists, rather
    -- than the key itself.
    --
    -- @since 0.1
    ShowKey
  | -- | Display the command itself, not the key.
    --
    -- @since 0.5
    HideKey
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via Supremum CmdDisplay

-- | @since 0.5
makePrisms ''CmdDisplay

-- | @since 0.5
instance DecodeTOML CmdDisplay where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> HideKey
      False -> ShowKey

-- | The different regions to apply truncation rules.
--
-- @since 0.1
data TruncRegion
  = -- | Apply truncation rules to commands/key names.
    --
    -- @since 0.1
    TCmdName
  | -- | Apply truncation rules to command log entire lines.
    --
    -- @since 0.1
    TCmdLine
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.5
makePrisms ''TruncRegion

-- | The maximum number of command characters to display in the logs.
--
-- @since 0.1
type Truncation :: TruncRegion -> Type
newtype Truncation a = MkTruncation
  { -- | @since 0.1
    unTruncation :: Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.5
      Num
    )
    via Natural

-- | @since 0.6
makeFieldLabelsNoPrefix ''Truncation

-- | @since 0.5
instance DecodeTOML (Truncation a) where
  tomlDecoder = MkTruncation <$> tomlDecoder

-- | Determines command log line truncation behavior. We need a separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
--
-- @since 0.1
data LineTruncation
  = -- | @since 0.1
    Undetected (Truncation 'TCmdLine)
  | -- | @since 0.1
    Detected
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.5
makePrisms ''LineTruncation

-- | @since 0.5
instance DecodeTOML LineTruncation where
  tomlDecoder = makeDecoder $ \case
    String "detect" -> pure Detected
    String bad -> invalidValue "Unexpected cmd-line-trunc. Only valid string is 'detect': " (String bad)
    Integer i
      | i >= 0 -> pure $ Undetected $ MkTruncation $ fromIntegral i
      | otherwise -> invalidValue "Unexpected cmd-line-trunc. Integers must be >= 0" (Integer i)
    badTy -> typeMismatch badTy

-- | Determines how we should treat control characters encountered in
-- logs.
--
-- @since 0.3
data StripControl
  = -- | \"Intelligently\" strip control characters e.g. colors are fine,
    -- ones that affect the cursor should be removed.
    --
    -- @since 0.3
    StripControlSmart
  | -- | Do not strip any control characters.
    --
    -- @since 0.3
    StripControlNone
  | -- | Strip all control characters.
    --
    -- @since 0.3
    StripControlAll
  deriving stock
    ( -- | @since 0.3
      Bounded,
      -- | @since 0.3
      Enum,
      -- | @since 0.3
      Eq,
      -- | @since 0.3
      Ord,
      -- | @since 0.3
      Show
    )
  deriving
    ( -- | @since 0.3
      Semigroup,
      -- | @since 0.3
      Monoid
    )
    via Supremum StripControl

-- | @since 0.5
makePrisms ''StripControl

-- | @since 0.5
instance DecodeTOML StripControl where
  tomlDecoder =
    tomlDecoder >>= \case
      "none" -> pure StripControlNone
      "smart" -> pure StripControlSmart
      "all" -> pure StripControlAll
      bad ->
        fail $
          "Unexpected strip-control. Expected one of none, smart, all: "
            <> unpack bad

-- | @since 0.6
data CmdLogging = MkCmdLogging
  { -- | @since 0.6
    stripControl :: !StripControl,
    -- | @since 0.6
    lineTrunc :: !(Maybe (Truncation 'TCmdLine))
  }
  deriving stock
    ( -- | @since 0.6
      Eq,
      -- | @since 0.6
      Show
    )

-- | @since 0.6
makeFieldLabelsNoPrefix ''CmdLogging

-- | @since 0.6
data FileLogging = MkFileLogging
  { -- | @since 0.6
    stripControl :: !StripControl,
    -- | @since 0.6
    log :: Tuple2 Handle (TBQueue FileLog)
  }

-- | @since 0.6
makeFieldLabelsNoPrefix ''FileLogging

-- | @since 0.6
instance Show FileLogging where
  showsPrec p fl =
    showParen (p > appPrec) $
      showString "MkFileLogging {stripControl = "
        . showsPrec appPrec1 (fl ^. #stripControl)
        . showString ", log = <(Handle, LogTextQueue)>"
        . showString "}"

-- | Holds logging data.
--
-- @since 0.7
data Logging r = MkLogging
  { -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.7
    cmdDisplay :: !CmdDisplay,
    -- | Truncates command names in the logs.
    --
    -- @since 0.7
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | Whether to log commands.
    --
    -- @since 0.7
    cmdLogging :: !(Maybe CmdLogging),
    -- | Console log queue.
    --
    -- @since 0.7
    consoleLogging :: TBQueue (LogRegion r),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.7
    fileLogging :: !(Maybe FileLogging)
  }

-- | @since 0.7
makeFieldLabelsNoPrefix ''Logging

-- | @since 0.7
instance Show (Logging r) where
  showsPrec p env =
    showParen (p > appPrec) $
      showString "MkEnv {cmdDisplay = "
        . showsPrec appPrec1 (env ^. #cmdDisplay)
        . showString ", cmdNameTrunc = "
        . showsPrec appPrec1 (env ^. #cmdNameTrunc)
        . showString ", cmdLogging = "
        . showsPrec appPrec1 (env ^. #cmdLogging)
        . showString ", consoleLogging = <TBQueue>"
        . showString ", fileLogging = "
        . showsPrec appPrec1 (env ^. #fileLogging)
        . showString "}"

-- | The commands themselves.
--
-- @since 0.1
class HasCommands env where
  -- | @since 0.1
  getCommands :: env -> NonEmptySeq Command

  -- | @since 0.1
  getCompletedCmds :: env -> TVar (Seq Command)

-- | Timeout, if any.
--
-- @since 0.1
class HasTimeout env where
  -- | @since 0.1
  getTimeout :: env -> Maybe Timeout

-- | Holds logging configuration.
--
-- @since 0.3
class HasLogging env r where
  -- | Retrieves logging env.
  --
  -- @since 0.1
  getLogging :: env -> Logging r

-- | The main 'Env' type used by Shrun. Intended to be used with
-- 'Shrun.Effects.MonadReader'.
--
-- @since 0.1
data Env = MkEnv
  { -- | Timeout.
    --
    -- @since 0.1
    timeout :: !(Maybe Timeout),
    -- | Logging env.
    --
    -- @since 0.7
    logging :: !(Logging ConsoleRegion),
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    --
    -- @since 0.1
    completedCmds :: !(TVar (Seq Command)),
    -- | The commands to run.
    --
    -- @since 0.1
    commands :: !(NonEmptySeq Command)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''Env

-- | @since 0.5
instance Show Env where
  showsPrec p env =
    showParen (p > appPrec) $
      showString "MkEnv {timeout = "
        . showsPrec appPrec1 (env ^. #timeout)
        . showString ", logging = "
        . showsPrec appPrec1 (env ^. #logging)
        . showString ", completedCmds = <TVar>"
        . showString ", commands = "
        . showsPrec appPrec1 (env ^. #commands)
        . showString "}"

-- | @since 0.1
instance HasTimeout Env where
  getTimeout = view #timeout

-- | @since 0.3
instance HasLogging Env ConsoleRegion where
  getLogging = view #logging

-- | @since 0.1
instance HasCommands Env where
  getCommands = view #commands
  getCompletedCmds = view #completedCmds
