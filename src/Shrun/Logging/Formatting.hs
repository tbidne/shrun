-- | Provides 'Log' formatting functionality.
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
    formatCommand,
    displayCmd,
    stripChars,
    brackets,
  )
where

import Data.Text qualified as T
import Effects.Time (getSystemTimeString)
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Data.Command (CommandP (MkCommandP), CommandP1)
import Shrun.Data.KeyHide (KeyHide (KeyHideOff))
import Shrun.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Data.Truncation (TruncRegion (TCmdName), Truncation)
import Shrun.Logging.Types
  ( Log,
    LogLevel
      ( LevelCommand,
        LevelError,
        LevelFatal,
        LevelFinished,
        LevelSuccess,
        LevelTimer,
        LevelWarn
      ),
  )
import Shrun.Logging.Types.Internal
  ( ConsoleLog (UnsafeConsoleLog),
    FileLog (UnsafeFileLog),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import System.Console.Pretty (Color (Blue, Cyan, Green, Red, White, Yellow))
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
formatConsoleLog ::
  KeyHide ->
  ConsoleLoggingEnv ->
  Log ->
  ConsoleLog
formatConsoleLog keyHide consoleLogging log =
  let line = case log ^. #cmd of
        -- FIXME: Line truncation is inconsistently applied here i.e. if the
        -- the log has a command or not. But that means all messages will have
        -- it applied _except_ command logs w/ an actual command name.
        --
        -- We should make line truncation apply to all logs but _always_ skip
        -- prefixes.
        Nothing -> brackets True prefix <> msgStripped
        Just cmd ->
          let cmd' =
                formatCommand
                  keyHide
                  (consoleLogging ^. #cmdNameTrunc)
                  cmd
           in -- truncate entire line if necessary
              truncateCmdLineFn
                $ mconcat
                  [ brackets False prefix,
                    cmd',
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
    msgStripped = stripChars (log ^. #msg) (consoleLogging ^. #stripControl)

    -- truncate entire line if necessary (flag on)
    truncateCmdLineFn =
      maybe
        id
        U.truncateIfNeeded
        (consoleLogging ^? #lineTrunc %? #unTruncation)

    colorize = P.color $ logToColor log
    prefix = logToPrefix log

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

-- | Formats a 'Log' into a 'FileLog'. Applies prefix and timestamp.
formatFileLog ::
  ( MonadTime m
  ) =>
  KeyHide ->
  FileLoggingEnv ->
  Log ->
  m FileLog
formatFileLog keyHide fileLogging log = do
  currTime <- getSystemTimeString
  let formatted = case log ^. #cmd of
        Nothing -> brackets True prefix <> msgStripped
        Just cmd ->
          let cmd' =
                formatCommand
                  keyHide
                  (fileLogging ^. #cmdNameTrunc)
                  cmd
           in mconcat
                [ brackets False prefix,
                  cmd',
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
    msgStripped = stripChars (log ^. #msg) stripControl
    stripControl = fileLogging ^. #stripControl
    prefix = logToPrefix log

formatCommand ::
  KeyHide ->
  Maybe (Truncation TCmdName) ->
  CommandP1 ->
  Text
formatCommand keyHide cmdNameTrunc com = brackets True (truncateNameFn cmdName)
  where
    -- Get cmd name to display. Always strip control sequences.
    cmdName =
      Utils.stripControlAll $ displayCmd com keyHide

    -- truncate cmd/name if necessary
    truncateNameFn =
      maybeApply
        U.truncateIfNeeded
        (cmdNameTrunc ^? (_Just % #unTruncation))

-- | Pretty show for 'Command'. If the command has a key, and 'KeyHide' is
-- 'KeyHideOff' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd (MkCommandP Nothing "some long command") KeyHideOn
-- "some long command"
--
-- >>> displayCmd (MkCommandP Nothing "some long command") KeyHideOff
-- "some long command"
--
-- >>> displayCmd (MkCommandP (Just "long") "some long command") KeyHideOn
-- "some long command"
--
-- >>> displayCmd (MkCommandP (Just "long") "some long command") KeyHideOff
-- "long"
displayCmd :: CommandP1 -> KeyHide -> Text
displayCmd (MkCommandP (Just key) _) KeyHideOff = key
displayCmd (MkCommandP _ cmd) _ = cmd

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': Strips whitespace + all control chars.
-- * 'StripControlSmart': Strips whitespace + 'ansi control' chars.
-- * 'StripControlNone': Strips whitespace.
stripChars :: Text -> StripControl -> Text
stripChars txt = \case
  StripControlAll -> Utils.stripControlAll txt
  -- whitespace
  StripControlNone -> T.strip txt
  StripControlSmart -> Utils.stripControlSmart txt
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
brackets :: Bool -> Text -> Text
brackets False s = "[" <> s <> "]"
brackets True s = "[" <> s <> "] "

-- | Transforms log to a color based on its 'LogLevel'.
logToColor :: Log -> Color
logToColor = levelToColor . view #lvl

-- | Transforms log to a prefix based on its 'LogLevel'.
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . view #lvl

-- | Maps 'LogLevel' to 'Color'.
levelToColor :: LogLevel -> Color
levelToColor LevelCommand = White
levelToColor LevelFinished = Blue
levelToColor LevelTimer = Cyan
levelToColor LevelSuccess = Green
levelToColor LevelWarn = Yellow
levelToColor LevelError = Red
levelToColor LevelFatal = Red

-- | Maps 'LogLevel' to \'Prefix\'.
levelToPrefix :: LogLevel -> Text
levelToPrefix LevelCommand = "Command"
levelToPrefix LevelFinished = "Finished"
levelToPrefix LevelTimer = "Timer"
levelToPrefix LevelSuccess = "Success"
levelToPrefix LevelWarn = "Warn"
levelToPrefix LevelError = "Error"
levelToPrefix LevelFatal = "Fatal"
