-- | Provides 'Log' formatting functionality.
--
-- @since 0.1
module Shrun.Logging.Formatting
  ( -- * High-level
    formatConsoleLog,
    formatFileLog,

    -- * Low-level
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,

    -- ** Utils
    displayCmd,
    stripChars,
    brackets,
  )
where

import Data.Text qualified as T
import Effects.MonadTime
  ( MonadTime (getSystemTime),
    formatLocalTime,
  )
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    FileLogging,
    Logging (..),
    StripControl (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Logging.Types.Internal
  ( ConsoleLog (UnsafeConsoleLog),
    FileLog (UnsafeFileLog),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import System.Console.Pretty (Color (..))
import System.Console.Pretty qualified as P

-- $setup
-- >>> import Shrun.Prelude
-- >>> import Shrun.Data.Command (Command (..))
-- >>> import Shrun.Configuration.Env.Types (CmdDisplay (..))

-- | Formats a log to be printed to the console.
--
-- @since 0.1
formatConsoleLog :: Logging r -> Log -> ConsoleLog
formatConsoleLog logging log =
  let line = case log ^. #cmd of
        Nothing -> brackets True prefix <> msgStripped
        Just com ->
          -- get cmd name to display
          let cmdName = displayCmd com (logging ^. #cmdDisplay)
              -- truncate cmd/name if necessary
              cmdName' = brackets True (truncateNameFn cmdName)
           in -- truncate entire line if necessary
              truncateCmdLineFn $
                mconcat
                  [ brackets False prefix,
                    cmdName',
                    msgStripped
                  ]
   in -- NOTE: We want colorize on the outside for two reasons:
      --
      -- 1. Truncation calculation should not take colorization into account,
      --    as chars are invisible.
      -- 2. Having colorization _inside_ can accidentally cause the "end color"
      --    chars to be stripped, leading to bugs where colorizing bleeds.
      --
      -- This 2nd point is likely the cause for some "color bleeding" that was
      -- occasionally noticed.
      UnsafeConsoleLog (colorize line)
  where
    msgStripped = stripChars (log ^. #msg) mStripControl
    mCmdLogging = logging ^. #cmdLogging
    mStripControl = mCmdLogging ^? _Just % #stripControl

    truncateNameFn =
      maybeApply
        U.truncateIfNeeded
        (logging ^? (#cmdNameTrunc %? #unTruncation))

    -- truncate entire line if necessary (flag on and command log only)
    truncateCmdLineFn = case ( log ^. #lvl,
                               mCmdLogging ^? (_Just % #lineTrunc %? #unTruncation)
                             ) of
      (LevelSubCommand, Just m) -> U.truncateIfNeeded m
      _ -> id

    colorize = P.color $ logToColor log
    prefix = logToPrefix log

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

-- | Formats a 'Log' into a 'FileLog'. Applies prefix and timestamp.
--
-- @since 0.7
formatFileLog ::
  ( MonadTime m
  ) =>
  FileLogging ->
  Log ->
  m FileLog
formatFileLog fileLogging log = do
  currTime <- formatLocalTime <$> getSystemTime
  let formatted = case log ^. #cmd of
        Nothing -> brackets True prefix <> msgStripped
        Just com ->
          mconcat
            [ brackets False prefix,
              brackets True (com ^. #command),
              msgStripped
            ]
      withTimestamp =
        mconcat
          [ brackets False (pack currTime),
            formatted,
            "\n"
          ]
  pure $ UnsafeFileLog withTimestamp
  where
    msgStripped = stripChars (log ^. #msg) (Just stripControl)
    stripControl = fileLogging ^. #stripControl
    prefix = logToPrefix log

-- | Pretty show for 'Command'. If the command has a key, and 'CmdDisplay' is
-- 'ShowKey' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd (MkCommand Nothing "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd (MkCommand Nothing "some long command") ShowKey
-- "some long command"
--
-- >>> displayCmd (MkCommand (Just "long") "some long command") HideKey
-- "some long command"
--
-- >>> displayCmd (MkCommand (Just "long") "some long command") ShowKey
-- "long"
--
-- @since 0.1
displayCmd :: Command -> CmdDisplay -> Text
displayCmd (MkCommand (Just key) _) ShowKey = key
displayCmd (MkCommand _ cmd) _ = cmd

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': Strips whitespace + all control chars.
-- * 'StripControlSmart': Strips whitespace + 'ansi control' chars.
-- * 'StripControlNone': Strips whitespace.
--
-- @since 0.3
stripChars :: Text -> Maybe StripControl -> Text
stripChars txt = \case
  Just StripControlAll -> Utils.stripControlAll txt
  -- whitespace
  Just StripControlNone -> T.strip txt
  --  default to smart
  _ -> Utils.stripControlSmart txt
{-# INLINE stripChars #-}

-- | Surrounds text with brackets, appending a space if the boolean is 'True'.
--
-- ==== __Examples__
--
-- >>> brackets False "text"
-- "[text]"
--
-- >>> brackets True "text"
-- "[text] "
--
-- @since 0.7
brackets :: Bool -> Text -> Text
brackets False s = "[" <> s <> "]"
brackets True s = "[" <> s <> "] "

-- | Transforms log to a color based on its 'LogLevel'.
--
-- @since 0.1
logToColor :: Log -> Color
logToColor = levelToColor . view #lvl
{-# INLINEABLE logToColor #-}

-- | Transforms log to a prefix based on its 'LogLevel'.
--
-- @since 0.1
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . view #lvl
{-# INLINEABLE logToPrefix #-}

-- | Maps 'LogLevel' to 'Color'.
--
-- @since 0.1
levelToColor :: LogLevel -> Color
levelToColor LevelSubCommand = White
levelToColor LevelFinished = Blue
levelToColor LevelTimer = Cyan
levelToColor LevelSuccess = Green
levelToColor LevelWarn = Yellow
levelToColor LevelError = Red
levelToColor LevelFatal = Red

-- | Maps 'LogLevel' to \'Prefix\'.
--
-- @since 0.1
levelToPrefix :: LogLevel -> Text
levelToPrefix LevelSubCommand = "Command"
levelToPrefix LevelFinished = "Finished"
levelToPrefix LevelTimer = "Timer"
levelToPrefix LevelSuccess = "Success"
levelToPrefix LevelWarn = "Warn"
levelToPrefix LevelError = "Error"
levelToPrefix LevelFatal = "Fatal"
