-- | Provides 'Log' formatting functionality.
module Shrun.Logging.Formatting
  ( -- * High-level
    formatConsoleLog,
    formatFileLog,

    -- ** Final logs
    formatConsoleMultiLineLogs,
    formatFileMultiLineLogs,

    -- * Low-level
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,

    -- ** Utils
    formatCommand,
    concatWithLineTrunc,
    displayCmd,
    stripChars,
    brackets,
    formatCommandText,
  )
where

import Data.Foldable qualified as F
import Data.List.NonEmpty qualified as NE
import Data.Monoid (Sum, getSum)
import Data.Text qualified as T
import Effects.Time (getSystemTimeString)
import Shrun.Command.Types
  ( CommandP1,
    CommandStatus
      ( CommandFailure,
        CommandRunning,
        CommandSuccess,
        CommandWaiting
      ),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (MkKeyHideSwitch),
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation
  ( TruncRegion
      ( TruncCommandName,
        TruncLine
      ),
    Truncation (MkTruncation),
  )
import Shrun.Configuration.Env.Types (HasCommands, getReadCommandStatus)
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Logging.Types
  ( Log,
    LogLevel
      ( LevelCommand,
        LevelDebug,
        LevelError,
        LevelFatal,
        LevelFinished,
        LevelKilled,
        LevelSuccess,
        LevelTimer,
        LevelWarn
      ),
    LogMessage,
  )
import Shrun.Logging.Types qualified as Types
import Shrun.Logging.Types.Internal
  ( ConsoleLog (UnsafeConsoleLog),
    FileLog (UnsafeFileLog),
  )
import Shrun.Prelude
import Shrun.Utils ((∸))
import Shrun.Utils qualified as Utils
import System.Console.Pretty (Color (Blue, Cyan, Green, Red, White, Yellow))
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
formatConsoleLog ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m
  ) =>
  KeyHideSwitch ->
  ConsoleLoggingEnv ->
  Log ->
  m ConsoleLog
formatConsoleLog keyHide consoleLogging log = do
  line <-
    coreFormatting
      False
      ((,Nothing) <$> consoleLogging ^. #lineTrunc)
      (consoleLogging ^. #commandNameTrunc)
      True
      (consoleLogging ^. #stripControl)
      keyHide
      log

  pure $ UnsafeConsoleLog (colorize line)
  where
    -- NOTE: We want colorize on the outside for two reasons:
    --
    -- 1. Truncation calculation should not take colorization into account,
    --    as chars are invisible.
    -- 2. Having colorization _inside_ can accidentally cause the "end color"
    --    chars to be stripped, leading to bugs where colorizing bleeds.
    --
    -- This 2nd point is likely the cause for some "color bleeding" that was
    -- occasionally noticed.
    colorize = P.color $ logToColor log

-- | Like 'formatConsoleLog', but for multiple logs. Concatenates all
-- together with a newline.
formatConsoleMultiLineLogs ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m
  ) =>
  KeyHideSwitch ->
  ConsoleLoggingEnv ->
  NonEmpty Log ->
  m ConsoleLog
formatConsoleMultiLineLogs keyHide consoleLogging logs@(l :| _) =
  fmap
    ( UnsafeConsoleLog
        -- No need to color each line individually: we can just do it once.
        . color
        . T.intercalate "\n"
        . F.toList
    )
    . traverse mkLine
    . zipMultilineSpacePrefix
    $ logs
  where
    mkLine (prefixSpace, log) =
      coreFormatting
        prefixSpace
        ((,Nothing) <$> consoleLogging ^. #lineTrunc)
        (consoleLogging ^. #commandNameTrunc)
        False
        (consoleLogging ^. #stripControl)
        keyHide
        log

    color = P.color (logToColor l)

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

-- | Formats a 'Log' into a 'FileLog'. Applies prefix and timestamp.
formatFileLog ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m,
    MonadTime m
  ) =>
  KeyHideSwitch ->
  FileLoggingEnv ->
  Log ->
  m FileLog
formatFileLog keyHide fileLogging log = do
  currTime <- getSystemTimeString
  let timestamp = brackets (pack currTime)
      timestampLen = T.length timestamp

  line <-
    coreFormatting
      False
      ((,Just timestampLen) <$> fileLogging ^. #lineTrunc)
      (fileLogging ^. #commandNameTrunc)
      False
      (fileLogging ^. #stripControl)
      keyHide
      log

  let withTimestamp =
        mconcat
          [ timestamp,
            line,
            "\n"
          ]

  pure $ UnsafeFileLog withTimestamp
{-# INLINEABLE formatFileLog #-}

zipMultilineSpacePrefix :: NonEmpty a -> NonEmpty (Bool, a)
zipMultilineSpacePrefix = NE.zip (False :| [True, True ..])

-- | Like 'formatFileLog', but for multiple logs. Concatenates all
-- together.
formatFileMultiLineLogs ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m,
    MonadTime m
  ) =>
  KeyHideSwitch ->
  FileLoggingEnv ->
  NonEmpty Log ->
  m FileLog
formatFileMultiLineLogs keyHide fileLogging logs = do
  currTime <- getSystemTimeString
  let timestamp = brackets (pack currTime)
      timestampLen = T.length timestamp

      mkLine (prefixSpace, log) = do
        let withTs =
              if prefixSpace
                then withNoTimestamp
                else withTimestamp
        withTs
          <$> coreFormatting
            prefixSpace
            ((,Just timestampLen) <$> fileLogging ^. #lineTrunc)
            (fileLogging ^. #commandNameTrunc)
            False
            (fileLogging ^. #stripControl)
            keyHide
            log

      withTimestamp line =
        mconcat
          [ timestamp,
            line,
            "\n"
          ]

      withNoTimestamp line =
        mconcat
          [ line,
            "\n"
          ]

  fmap
    ( UnsafeFileLog
        . mconcat
        . F.toList
    )
    . traverse mkLine
    . zipMultilineSpacePrefix
    $ logs
{-# INLINEABLE formatFileMultiLineLogs #-}

-- | Core formatting, shared by console and file logs. Basic idea:
--
-- 1. If the log contains a command, it is formatted according to
--    'formatCommand' and command name truncation.
--
-- 2. The message is stripped of control chars according to strip control.
--
-- 3. Line truncation is applied if applicable. Note this applies only to
--    the stripped message. The prefix (e.g. level label, timestamp, command
--    name) are always present, though they __do__ count towards the
--    truncation count. I.e. if the prefixes add up to 10 chars, and the
--    line truncation is 15, then we only have 5 chars for the message before
--    truncation kicks in.
coreFormatting ::
  ( HasCallStack,
    HasCommands env,
    MonadAtomic m,
    MonadReader env m
  ) =>
  -- | If true, the prefix is replaced with whitespace. This is for multiline,
  -- final logs, where we only want the prefix on the first line. Normal usage
  -- includes the prefix.
  Bool ->
  -- | Optional line truncation. If we have some line truncation then there
  -- is a further optional "prefix length". This is so that file logging
  -- can pass in the timestamp length so it is taken into account
  -- (command logging has no special prefix besides ANSI codes, which is
  -- ignored).
  Maybe (Truncation TruncLine, Maybe Int) ->
  -- | Optional cmd name truncation
  Maybe (Truncation TruncCommandName) ->
  -- | If true, strips leading whitespace. This is so that file logging
  -- can preserve leading whitespace (so that the file output retains
  -- potential alignment; alignment is irrelevant to console logs).
  --
  -- This is for leading only, not trailing, as the latter is irrelevant to
  -- formatting, and we may want trailing whitespace if we ever concat two
  -- logs together.
  Bool ->
  -- | Strip control
  StripControl t ->
  -- | Key hide
  KeyHideSwitch ->
  -- | Log to format
  Log ->
  m Text
coreFormatting
  spacePrefix
  mLineTrunc
  mCommandNameTrunc
  stripLeading
  stripControl
  keyHide
  log = do
    statusPrefix <- case log ^. #lvl of
      LevelTimer -> mkStatus
      LevelFinished -> mkStatus
      _ -> pure ""

    let -- prefix is something like "[Success] " or "[Command][some cmd] ".
        -- Notice this does not include ANSI codes or a timestamp.
        prefix =
          mconcat
            [ brackets logPrefix,
              statusPrefix,
              cmdPrefix,
              " "
            ]

        finalPrefix =
          if spacePrefix
            then "  "
            else prefix

        msgStripControlled = stripChars (log ^. #msg) stripControl
        msgStripped =
          if stripLeading
            then Types.unsafeMapLogMessage T.stripStart msgStripControlled
            else msgStripControlled

        logPrefix = logToPrefix log

        cmdPrefix = case log ^. #cmd of
          Nothing -> ""
          Just cmd ->
            formatCommand keyHide mCommandNameTrunc cmd ^. #unUnlinedText

    pure $ concatWithLineTrunc mLineTrunc finalPrefix (msgStripped ^. #unLogMessage)
    where
      mkStatus = do
        statusMap <- getReadCommandStatus
        let countStatuses (_, status) = case status of
              CommandWaiting -> (1, 0, 0, 0)
              CommandRunning _ -> (0, 1, 0, 0)
              CommandFailure -> (0, 0, 1, 0)
              CommandSuccess -> (0, 0, 0, 1)

            (w, r, f, s) = foldMap countStatuses statusMap

        pure
          $ brackets
          $ mconcat
            [ tos w,
              "|",
              tos r,
              "|",
              tos f,
              "|",
              tos s
            ]

      tos :: Sum Int -> Text
      tos = showt . getSum

formatCommand ::
  KeyHideSwitch ->
  Maybe (Truncation TruncCommandName) ->
  CommandP1 ->
  UnlinedText
formatCommand keyHide commandNameTrunc com =
  ShrunText.reallyUnsafeMap (brackets . truncateNameFn) cmdName
  where
    -- Get cmd name to display. Always strip control sequences. Futhermore,
    -- strip leading/trailing whitespace.
    cmdName = displayCmd com keyHide

    -- truncate cmd/name if necessary
    truncateNameFn =
      maybeApply
        Utils.truncateIfNeeded
        (commandNameTrunc ^? (_Just % #unTruncation))

-- | Replace newlines with whitespace before stripping, so any strings
-- separated by newlines do not get smashed together.
formatCommandText :: Text -> UnlinedText
formatCommandText =
  ShrunText.reallyUnsafeMap (T.strip . Utils.stripControlAll)
    . ShrunText.fromTextReplace

-- | Combines a prefix @p@ and msg @m@ with possible line truncation. If no
-- truncation is given then concatWithLineTrunc is equivalent to @p <> m@.
-- If we are given some line truncation @l@, then we derive
--
-- @
--    k := l - prefix_len -- k is clamped to zero
-- @
--
-- and return
--
-- @
--    prefix <> t'
-- @
--
-- where @t'@ is @t@ truncated to @k@ chars. Notice the prefix is always
-- included untarnished.
concatWithLineTrunc ::
  Maybe (Truncation TruncLine, Maybe Int) ->
  Text ->
  Text ->
  Text
concatWithLineTrunc Nothing prefix msg = prefix <> msg
concatWithLineTrunc (Just (MkTruncation lineTrunc, mPrefixLen)) prefix msg =
  prefix <> Utils.truncateIfNeeded lineTrunc' msg
  where
    lineTrunc' =
      lineTrunc ∸ (prefixLen + T.length prefix)

    prefixLen = fromMaybe 0 mPrefixLen

-- | Pretty show for 'Command'. If the command has a key, and 'KeyHideSwitch' is
-- 'KeyHideOff' then we return the key. Otherwise we return the command itself.
--
-- >>> import Shrun.Command.Types (CommandP (MkCommandP), unsafeFromInt)
-- >>> import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch (KeyHideSwitch (MkKeyHideSwitch))
--
-- >>> let idx = unsafeFromInt 1
-- >>> let mkCmd = MkCommandP idx
-- >>> let fmt k cmd kh = view #unUnlinedText $ displayCmd (mkCmd k cmd) kh
--
-- >>> fmt Nothing "some long command" (MkKeyHideSwitch true)
-- "some long command"
--
-- >>> fmt Nothing "some long command" (MkKeyHideSwitch false)
-- "some long command"
--
-- >>> fmt (Just "long") "some long command" (MkKeyHideSwitch true)
-- "some long command"
--
-- >>> fmt (Just "long") "some long command" (MkKeyHideSwitch false)
-- "long"
displayCmd :: CommandP1 -> KeyHideSwitch -> UnlinedText
displayCmd cmd kh = case (cmd ^. #key, kh) of
  (Just key, MkKeyHideSwitch False) -> formatCommandText key
  _ -> formatCommandText (cmd ^. #command)

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': All control chars.
-- * 'StripControlSmart': Ansi control chars.
-- * 'StripControlNone': Nothing.
stripChars :: LogMessage -> StripControl t -> LogMessage
stripChars txt =
  \case
    -- Coerce is needed as stripControl operators on UnlinedText. Originally
    -- this was convenient as our log was UnlinedText, but now it is
    -- LogMessage.
    StripControlAll -> coerce Utils.stripControlAll txt
    StripControlNone -> txt
    StripControlSmart -> coerce Utils.stripControlSmart txt
{-# INLINE stripChars #-}

-- | Surrounds text with brackets, appending a space if the boolean is 'True'.
--
-- ==== __Examples__
--
-- >>> brackets "text"
-- "[text]"
brackets :: Text -> Text
brackets s = "[" <> s <> "]"

-- | Transforms log to a color based on its 'LogLevel'.
logToColor :: Log -> Color
logToColor = levelToColor . view #lvl

-- | Transforms log to a prefix based on its 'LogLevel'.
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . view #lvl

-- | Maps 'LogLevel' to 'Color'.
levelToColor :: LogLevel -> Color
levelToColor LevelDebug = White
levelToColor LevelCommand = White
levelToColor LevelFinished = Blue
levelToColor LevelTimer = Cyan
levelToColor LevelSuccess = Green
levelToColor LevelWarn = Yellow
levelToColor LevelError = Red
levelToColor LevelFatal = Red
levelToColor LevelKilled = Red

-- | Maps 'LogLevel' to \'Prefix\'.
levelToPrefix :: LogLevel -> Text
levelToPrefix LevelDebug = "Debug"
levelToPrefix LevelCommand = "Command"
levelToPrefix LevelFinished = "Finished"
levelToPrefix LevelTimer = "Status"
levelToPrefix LevelSuccess = "Success"
levelToPrefix LevelWarn = "Warn"
levelToPrefix LevelError = "Error"
levelToPrefix LevelFatal = "Fatal"
levelToPrefix LevelKilled = "Killed"
