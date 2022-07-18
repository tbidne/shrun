{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and typeclasses for our environment.
--
-- @since 0.5
module ShellRun.Configuration.Env.Types
  ( -- * \"HasX\" style typeclasses
    HasCommands (..),
    HasLogging (..),
    HasCompletedCmds (..),
    HasTimeout (..),

    -- * Types
    Env (..),
    CmdDisplay (..),
    CmdLogging (..),
    Truncation (..),
    LineTruncation (..),
    TruncRegion (..),
    StripControl (..),

    -- * Optics
    _HideKey,
    _ShowKey,
    _Disabled,
    _Enabled,
    _TCmdName,
    _TCmdLine,
    _MkTruncation,
    _StripControlSmart,
    _StripControlAll,
    _StripControlNone,
    _Undetected,
    _Detected,
  )
where

import Data.Sequence (Seq)
import GHC.Show (appPrec, appPrec1)
import ShellRun.Data.Command (Command)
import ShellRun.Data.NonEmptySeq (NonEmptySeq)
import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Logging.Queue (LogTextQueue)
import ShellRun.Prelude
import Text.Show (showParen, showString)

-- | Type for determining if we stream commands' logs.
--
-- @since 0.1
data CmdLogging
  = -- | No logging of sub-commands.
    --
    -- @since 0.1
    Disabled
  | -- | Logging of sub-commands
    --
    -- @since 0.1
    Enabled
  deriving stock
    ( -- | @since 0.1
      Bounded,
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
    via Supremum CmdLogging

-- | @since 0.5
makePrisms ''CmdLogging

-- | @since 0.5
instance DecodeTOML CmdLogging where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> Enabled
      False -> Disabled

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
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

-- | @since 0.5
makePrisms ''Truncation

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

-- | The commands themselves.
--
-- @since 0.1
class HasCommands env where
  -- | @since 0.1
  getCommands :: env -> NonEmptySeq Command

-- | Timeout, if any.
--
-- @since 0.1
class HasTimeout env where
  -- | @since 0.1
  getTimeout :: env -> Maybe Timeout

-- | Holds logging configuration.
--
-- @since 0.3
class HasLogging env where
  -- | Determines how to display command names.
  --
  -- @since 0.1
  getCmdDisplay :: env -> CmdDisplay

  -- | Determines command line truncation behavior.
  --
  -- @since 0.1
  getCmdLineTrunc :: env -> Maybe (Truncation 'TCmdLine)

  -- | Determines if we should log commands' output to the console.
  --
  -- @since 0.1
  getCmdLogging :: env -> CmdLogging

  -- | Determines command name truncation behavior.
  --
  -- @since 0.1
  getCmdNameTrunc :: env -> Maybe (Truncation 'TCmdName)

  -- | File logging, if any.
  --
  -- @since 0.3
  getFileLogging :: env -> Maybe (FilePath, LogTextQueue)

  -- | Determines if logging is enabled globally.
  --
  -- @since 0.1
  getDisableLogging :: env -> Bool

  -- | Determines control character behavior.
  --
  -- @since 0.3
  getStripControl :: env -> StripControl

-- | Determines command line truncation behavior.
--
-- @since 0.1
class HasCompletedCmds env where
  -- | @since 0.1
  getCompletedCmds :: env -> TVar (Seq Command)

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Effects.MonadReader'.
--
-- @since 0.1
data Env = MkEnv
  { -- | Timeout.
    --
    -- @since 0.1
    timeout :: !(Maybe Timeout),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.1
    fileLogging :: !(Maybe (Tuple2 FilePath LogTextQueue)),
    -- | Whether to log commands.
    --
    -- @since 0.1
    cmdLogging :: !CmdLogging,
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.1
    cmdDisplay :: !CmdDisplay,
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1
    cmdLineTrunc :: !(Maybe (Truncation 'TCmdLine)),
    -- | Determines to what extent we should remove control characters
    -- from logs.
    --
    -- @since 0.3
    stripControl :: !StripControl,
    -- | Holds a sequence of commands that have completed. Used so we can
    -- determine which commands have /not/ completed if we time out.
    --
    -- @since 0.1
    completedCmds :: !(TVar (Seq Command)),
    -- | Overarching option for logging. If it is false then all logging is
    -- disabled.
    --
    -- @since 0.1
    disableLogging :: !Bool,
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
        . showString ", fileLogging = "
        . showsPrec appPrec1 (env ^. #fileLogging)
        . showString ", cmdLogging = "
        . showsPrec appPrec1 (env ^. #cmdLogging)
        . showString ", cmdDisplay = "
        . showsPrec appPrec1 (env ^. #cmdDisplay)
        . showString ", cmdNameTrunc = "
        . showsPrec appPrec1 (env ^. #cmdNameTrunc)
        . showString ", cmdLineTrunc = "
        . showsPrec appPrec1 (env ^. #cmdLineTrunc)
        . showString ", stripControl = "
        . showsPrec appPrec1 (env ^. #stripControl)
        . showString ", completedCmds = <TVar>"
        . showString ", disableLogging = "
        . showsPrec appPrec1 (env ^. #disableLogging)
        . showString ", commands = "
        . showsPrec appPrec1 (env ^. #commands)
        . showString "}"

-- | @since 0.1
instance HasTimeout Env where
  getTimeout = view #timeout
  {-# INLINE getTimeout #-}

-- | @since 0.3
instance HasLogging Env where
  getCmdDisplay = view #cmdDisplay
  getCmdLineTrunc = view #cmdLineTrunc
  getCmdLogging = view #cmdLogging
  getCmdNameTrunc = view #cmdNameTrunc
  getFileLogging = view #fileLogging
  getDisableLogging = view #disableLogging
  getStripControl = view #stripControl
  {-# INLINE getCmdDisplay #-}
  {-# INLINE getCmdLineTrunc #-}
  {-# INLINE getCmdLogging #-}
  {-# INLINE getCmdNameTrunc #-}
  {-# INLINE getFileLogging #-}
  {-# INLINE getDisableLogging #-}
  {-# INLINE getStripControl #-}

-- | @since 0.1
instance HasCompletedCmds Env where
  getCompletedCmds = view #completedCmds
  {-# INLINE getCompletedCmds #-}

-- | @since 0.1
instance HasCommands Env where
  getCommands = view #commands
  {-# INLINE getCommands #-}
