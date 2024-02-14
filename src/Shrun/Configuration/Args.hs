{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command line arguments.
module Shrun.Configuration.Args
  ( Args (..),
    FileMode (..),
    FileSizeMode (..),
    defaultArgs,
    parserInfoArgs,
  )
where

import Data.Char qualified as Ch
import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Time.Relative qualified as RelativeTime
import Data.Version (Version (versionBranch))
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.Optparse (validOsPath)
import Options.Applicative
  ( ParseError (ErrorMsg),
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    ReadM,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_shrun qualified as Paths
import Shrun.Configuration.Args.TH (getDefaultConfigTH)
import Shrun.Configuration.Env.Types
  ( KeyHide (KeyHideOn),
    LineTruncation (Detected, Undetected),
    StripControl (StripControlAll, StripControlNone, StripControlSmart),
    TruncRegion (TCmdName),
    Truncation (MkTruncation),
  )
import Shrun.Data.FilePathDefault (FilePathDefault (FPDefault, FPManual))
import Shrun.Data.PollInterval (PollInterval (MkPollInterval), defaultPollInterval)
import Shrun.Data.Timeout (Timeout (MkTimeout))
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.TimerFormat qualified as TimerFormat
import Shrun.Notify.Types
  ( NotifyAction,
    NotifySystemP1,
    NotifyTimeout,
  )
import Shrun.Notify.Types qualified as Notify
import Shrun.Prelude
import Shrun.Utils qualified as U
import Text.Read qualified as TR

-- | File mode.
data FileMode
  = FileModeAppend
  | FileModeWrite
  deriving stock (Eq, Show)

instance DecodeTOML FileMode where
  tomlDecoder =
    tomlDecoder @Text >>= \case
      "append" -> pure FileModeAppend
      "write" -> pure FileModeWrite
      bad -> fail $ "Unrecognized file-mode: " <> unpack bad

parseFileSizeMode :: (MonadFail m) => Text -> m FileSizeMode
parseFileSizeMode txt = do
  let (m, byteTxt) = T.break Ch.isSpace txt
  cons <- case m of
    "warn" -> pure FileSizeModeWarn
    "delete" -> pure FileSizeModeDelete
    bad -> fail $ "Unrecognized file-log-size-mode: " <> unpack bad
  case U.parseByteText byteTxt of
    Right b -> pure $ cons b
    Left err -> fail $ "Could not parse --file-log-size-mode size: " <> unpack err

-- | Determines what to do if the log file surpasses the given size
-- threshold.
data FileSizeMode
  = -- | Print a warning.
    FileSizeModeWarn (Bytes B Natural)
  | -- | Delete the file.
    FileSizeModeDelete (Bytes B Natural)
  deriving stock (Eq, Show)

instance DecodeTOML FileSizeMode where
  tomlDecoder = tomlDecoder >>= parseFileSizeMode

-- | Type for parsing command line args.
data Args = MkArgs
  { -- | Optional config file.
    configPath :: Maybe OsPath,
    -- | Ignores toml config file.
    noConfig :: Bool,
    -- | Timeout.
    timeout :: Maybe Timeout,
    -- | Disables timeout.
    noTimeout :: Bool,
    -- | Shell logic to run before each command.
    init :: Maybe Text,
    -- | Disables init.
    noInit :: Bool,
    -- | Whether to display command by (key) name or command.
    keyHide :: Maybe KeyHide,
    -- | Disables keyHide.
    noKeyHide :: Bool,
    -- | How often to poll commands for logs, in microseconds.
    pollInterval :: Maybe PollInterval,
    -- | Disables pollInterval.
    noPollInterval :: Bool,
    -- | Determines the max log size we read from commands in one go.
    cmdLogSize :: Maybe (Bytes B Natural),
    -- | Disables cmdLogSize.
    noCmdLogSize :: Bool,
    -- | How to format the timer.
    timerFormat :: Maybe TimerFormat,
    -- | Disables timerFormat.
    noTimerFormat :: Bool,
    -- | The max number of command characters to display in the logs.
    cmdNameTrunc :: Maybe (Truncation TCmdName),
    -- | Disables cmdNameTrunc.
    noCmdNameTrunc :: Bool,
    -- | Whether to log commands.
    cmdLog :: Maybe Bool,
    -- | Disables cmdLogging.
    noCmdLog :: Bool,
    -- | Determines to what extent we should remove control characters
    -- from command logs.
    cmdLogStripControl :: Maybe StripControl,
    -- | Disables cmdLogStripControl.
    noCmdLogStripControl :: Bool,
    -- | The max number of line characters to display in the logs.
    cmdLogLineTrunc :: Maybe LineTruncation,
    -- | Disables cmdLogLineTrunc.
    noCmdLogLineTrunc :: Bool,
    -- | Optional path to log file. Determines if we log to a file.
    fileLog :: Maybe FilePathDefault,
    -- | Disable fileLog.
    noFileLog :: Bool,
    -- | Determines to what extent we should remove control characters
    -- from file logs.
    fileLogStripControl :: Maybe StripControl,
    -- | Disables fileLogStripControl.
    noFileLogStripControl :: Bool,
    -- | Mode to use with the file log.
    fileLogMode :: Maybe FileMode,
    -- | Disables fileLogMode.
    noFileLogMode :: Bool,
    -- | Threshold for when we should warn about the log file size.
    fileLogSizeMode :: Maybe FileSizeMode,
    -- | Disables fileLogSizeMode.
    noFileLogSizeMode :: Bool,
    -- | Actions for which to send notifications.
    notifyAction :: Maybe NotifyAction,
    -- | Disables notifyAction.
    noNotifyAction :: Bool,
    -- | The notification system to use.
    notifySystem :: Maybe NotifySystemP1,
    -- | Disables notifySystem.
    noNotifySystem :: Bool,
    -- | when to timeout successful notifications.
    notifyTimeout :: Maybe NotifyTimeout,
    -- | Disables notifyTimeout.
    noNotifyTimeout :: Bool,
    -- | List of commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Args

-- | Default configuration.
defaultArgs :: NESeq Text -> Args
defaultArgs cmds =
  MkArgs
    { timeout = empty,
      noTimeout = False,
      configPath = empty,
      noConfig = False,
      keyHide = empty,
      noKeyHide = False,
      pollInterval = empty,
      noPollInterval = False,
      timerFormat = empty,
      noTimerFormat = False,
      init = empty,
      noInit = False,
      cmdLogSize = empty,
      noCmdLogSize = False,
      cmdLog = empty,
      noCmdLog = False,
      cmdLogStripControl = empty,
      noCmdLogStripControl = False,
      cmdNameTrunc = empty,
      noCmdNameTrunc = False,
      cmdLogLineTrunc = empty,
      noCmdLogLineTrunc = False,
      fileLog = empty,
      noFileLog = False,
      fileLogStripControl = empty,
      noFileLogStripControl = False,
      fileLogMode = empty,
      noFileLogMode = False,
      fileLogSizeMode = empty,
      noFileLogSizeMode = False,
      notifySystem = empty,
      noNotifySystem = False,
      notifyAction = empty,
      noNotifyAction = False,
      notifyTimeout = empty,
      noNotifyTimeout = False,
      commands = cmds
    }

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Shrun: A tool for running shell commands concurrently."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Shrun runs shell commands concurrently. In addition to providing ",
            "basic timing and logging functionality, we also provide the ",
            "ability to pass in a config file that can be used to define ",
            "aliases for commands. See github.com/tbidne/shrun#README for ",
            "full documentation."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> configParser
    <*> noConfigParser
    <*> timeoutParser
    <*> noTimeoutParser
    <*> initParser
    <*> noInitParser
    <*> commandDisplayParser
    <*> noKeyHideParser
    <*> pollIntervalParser
    <*> noPollIntervalParser
    <*> cmdLogSizeParser
    <*> noCmdLogSizeParser
    <*> timerFormatParser
    <*> noTimerFormatParser
    <*> cmdNameTruncParser
    <*> noCmdNameTruncParser
    <*> cmdLogParser
    <*> noCmdLogParser
    <*> cmdLogStripControlParser
    <*> noCmdLogStripControlParser
    <*> cmdLogLineTruncParser
    <*> noCmdLogLineTruncParser
    <*> fileLogParser
    <*> noFileLogParser
    <*> fileLogStripControlParser
    <*> noFileLogStripControlParser
    <*> fileLogModeParser
    <*> noFileLogModeParser
    <*> fileLogSizeModeParser
    <*> noFileLogSizeModeParser
    <*> notifyActionParser
    <*> noNotifyActionParser
    <*> notifySystemParser
    <*> noNotifySystemParser
    <*> notifyTimeoutParser
    <*> noNotifyTimeoutParser
    <**> defaultConfig
    <**> version
    <**> OA.helper
    <*> commandsParser

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

defaultConfig :: Parser (a -> a)
defaultConfig = OA.infoOption (unpack txt) (OA.long "default-config" <> mkHelp help)
  where
    txt = T.unlines $$getDefaultConfigTH
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (Maybe OsPath)
configParser =
  OA.optional
    $ OA.option
      validOsPath
      ( mconcat
          [ OA.long "config",
            OA.short 'c',
            mkHelp helpTxt,
            OA.metavar "PATH"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Path to TOML config file. If this argument is not given ",
          "we automatically look in the XDG config directory ",
          "e.g. ~/.config/shrun/config.toml"
        ]

noConfigParser :: Parser Bool
noConfigParser =
  OA.flag
    False
    True
    ( mconcat
        [ OA.long "no-config",
          OA.hidden,
          mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "Overrides toml file config regardless of how it was obtained i.e. ",
          "explicit --config or implicit reading of the XDG config file. ",
          "Used for when a config file exists at the expected XDG ",
          "location, but we want to ignore it."
        ]

timeoutParser :: Parser (Maybe Timeout)
timeoutParser =
  OA.optional
    $ OA.option
      readTimeout
      ( mconcat
          [ OA.long "timeout",
            OA.short 't',
            mkHelp helpTxt,
            OA.metavar "(NATURAL | STRING)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

readTimeout :: ReadM Timeout
readTimeout = do
  s <- OA.str
  case RelativeTime.fromString s of
    Left _ ->
      OA.readerAbort
        $ ErrorMsg
        $ "Could not parse timeout: "
        <> s
    Right t -> pure $ MkTimeout $ RelativeTime.toSeconds t

noTimeoutParser :: Parser Bool
noTimeoutParser =
  OA.switch
    $ mconcat
      [ OA.long "no-timeout",
        OA.hidden,
        mkHelp "Disables --timeout."
      ]

cmdNameTruncParser :: Parser (Maybe (Truncation TCmdName))
cmdNameTruncParser =
  OA.optional
    $ OA.option
      readTruncation
      ( mconcat
          [ OA.long "cmd-name-trunc",
            OA.short 'x',
            mkHelp helpTxt,
            OA.metavar "NATURAL"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation. This affects ",
          "everywhere the command/key-name shows up (i.e. in command logs or ",
          "final success/error message); File logs created via --file-log ",
          "are unaffected."
        ]

noCmdNameTruncParser :: Parser Bool
noCmdNameTruncParser =
  OA.switch
    $ mconcat
      [ OA.long "no-cmd-name-trunc",
        mkHelp "Disables --cmd-name-trunc."
      ]

cmdLogSizeParser :: Parser (Maybe (Bytes B Natural))
cmdLogSizeParser =
  OA.optional
    $ OA.option
      readCmdLogSize
      ( mconcat
          [ OA.long "cmd-log-size",
            mkHelp helpTxt,
            OA.metavar "NATURAL"
          ]
      )
  where
    readCmdLogSize = MkBytes <$> OA.auto
    helpTxt =
      mconcat
        [ "Non-negative integer that determines the size (bytes) of command ",
          "logs in a single read (--cmd-log and --file-log). Logs larger than ",
          "--cmd-log-size will be read in a subsequent read, hence broken ",
          "across lines. The default is 1024."
        ]

noCmdLogSizeParser :: Parser Bool
noCmdLogSizeParser =
  OA.switch
    $ mconcat
      [ OA.long "no-cmd-log-size",
        mkHelp "Disables --cmd-log-size."
      ]

cmdLogLineTruncParser :: Parser (Maybe LineTruncation)
cmdLogLineTruncParser =
  OA.optional
    $ OA.option
      (defRead <|> readDetectTruncation)
      ( mconcat
          [ OA.long "cmd-log-line-trunc",
            OA.short 'y',
            mkHelp helpTxt,
            OA.metavar "(NATURAL | detect)"
          ]
      )
  where
    defRead = Undetected <$> readTruncation
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of logs ",
          "produced via --cmd-log in the console logs. Can also be the ",
          "string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation. This does ",
          "not affect file logs with --file-log."
        ]

noCmdLogLineTruncParser :: Parser Bool
noCmdLogLineTruncParser =
  OA.switch
    $ mconcat
      [ OA.long "no-cmd-log-line-trunc",
        OA.hidden,
        mkHelp "Disables --cmd-log-line-trunc."
      ]

readTruncation :: ReadM (Truncation a)
readTruncation = MkTruncation <$> OA.auto

readDetectTruncation :: ReadM LineTruncation
readDetectTruncation =
  OA.str >>= T.toCaseFold .> \case
    "detect" -> pure Detected
    bad ->
      OA.readerAbort
        $ ErrorMsg
        $ "Unrecognized truncation option:"
        <> unpack bad

cmdLogStripControlParser :: Parser (Maybe StripControl)
cmdLogStripControlParser =
  OA.optional
    $ OA.option
      readStripControl
      ( mconcat
          [ OA.long "cmd-log-strip-control",
            OA.short 's',
            mkHelp helpTxt,
            OA.metavar "(all | smart | none)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Control characters can wreak layout havoc with the --cmd-log",
          " option, thus we include this option. 'all' strips all",
          " such chars. 'none' does nothing i.e. all chars are left",
          " untouched. The default 'smart' attempts to strip",
          " only the control chars that affect layout (e.g. cursor movements) and",
          " leaves others unaffected (e.g. colors). This has the potential",
          " to be the 'prettiest' though it is possible to miss some chars.",
          " This option is experimental and subject to change."
        ]

readStripControl :: ReadM StripControl
readStripControl =
  OA.str >>= T.toCaseFold .> \case
    "all" -> pure StripControlAll
    "none" -> pure StripControlNone
    "smart" -> pure StripControlSmart
    bad ->
      OA.readerAbort
        $ ErrorMsg
        $ "Unrecognized strip-control option: "
        <> unpack bad

noCmdLogStripControlParser :: Parser Bool
noCmdLogStripControlParser =
  OA.switch
    $ mconcat
      [ OA.long "no-cmd-log-strip-control",
        OA.hidden,
        mkHelp "Disables --cmd-log-strip-control."
      ]

fileLogParser :: Parser (Maybe FilePathDefault)
fileLogParser =
  OA.optional
    $ OA.option
      readLogFile
      ( mconcat
          [ OA.long "file-log",
            OA.short 'f',
            mkHelp helpTxt,
            OA.metavar "(default | PATH)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "If a path is supplied, all logs will additionally be written to ",
          "the supplied file. Furthermore, command logs will be written to ",
          "the file irrespective of --cmd-log. Console logging is unaffected. ",
          "This can be useful for investigating command failures. ",
          "If the string 'default' is given, then we write to the XDG config ",
          "directory e.g. ~/.config/shrun/shrun.log."
        ]

noFileLogParser :: Parser Bool
noFileLogParser =
  OA.switch
    $ mconcat
      [ OA.long "no-file-log",
        OA.hidden,
        mkHelp "Disables --file-log."
      ]

readLogFile :: ReadM FilePathDefault
readLogFile = do
  f <- OA.str
  case fmap Ch.toLower f of
    "default" -> pure FPDefault
    "" -> fail "Empty path given for --file-log"
    _ -> FPManual <$> FsUtils.encodeFpToOsFail f

fileLogModeParser :: Parser (Maybe FileMode)
fileLogModeParser =
  OA.optional
    $ OA.option
      readFileMode
      ( mconcat
          [ OA.long "file-log-mode",
            mkHelp helpTxt,
            OA.metavar "(append | write)"
          ]
      )
  where
    helpTxt = "Mode in which to open the log file. Defaults to write."

noFileLogModeParser :: Parser Bool
noFileLogModeParser =
  OA.switch
    $ mconcat
      [ OA.long "no-file-log-mode",
        OA.hidden,
        mkHelp "Disables --file-log-mode."
      ]

readFileMode :: ReadM FileMode
readFileMode =
  OA.str >>= \case
    "append" -> pure FileModeAppend
    "write" -> pure FileModeWrite
    bad ->
      OA.readerAbort
        $ ErrorMsg
        $ "Unrecognized --file-log-mode option: "
        <> unpack bad

fileLogStripControlParser :: Parser (Maybe StripControl)
fileLogStripControlParser =
  OA.optional
    $ OA.option
      readStripControl
      ( mconcat
          [ OA.long "file-log-strip-control",
            mkHelp helpTxt,
            OA.metavar "(all | smart | none)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "--cmd-log-strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

noFileLogStripControlParser :: Parser Bool
noFileLogStripControlParser =
  OA.switch
    $ mconcat
      [ OA.long "no-file-log-strip-control",
        OA.hidden,
        mkHelp "Disables --file-log-strip-control."
      ]

fileLogSizeModeParser :: Parser (Maybe FileSizeMode)
fileLogSizeModeParser =
  OA.optional
    $ OA.option
      readFileSize
      ( mconcat
          [ OA.long "file-log-size-mode",
            mkHelp helpTxt,
            OA.metavar "(warn SIZE | delete SIZE)"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Sets a threshold for the file log size, upon which we either ",
          "print a warning or delete the file, if it is exceeded. ",
          "The SIZE should include the value and units e.g. ",
          "warn 10 mb, warn 5 gigabytes, delete 20.5B."
        ]
    readFileSize = OA.str >>= parseFileSizeMode

noFileLogSizeModeParser :: Parser Bool
noFileLogSizeModeParser =
  OA.switch
    $ mconcat
      [ OA.long "no-file-log-size-mode",
        OA.hidden,
        mkHelp "Disables --file-log-size-mode."
      ]

cmdLogParser :: Parser (Maybe Bool)
cmdLogParser =
  OA.optional
    $ OA.flag'
      True
      ( mconcat
          [ OA.short 'l',
            OA.long "cmd-log",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "The default behavior is to swallow logs for the commands ",
          "themselves. This flag gives each command a console region in ",
          "which its logs will be printed. Only the latest log per region ",
          "is show at a given time."
        ]

noCmdLogParser :: Parser Bool
noCmdLogParser =
  OA.switch
    $ mconcat
      [ OA.long "no-cmd-log",
        OA.hidden,
        mkHelp "Disables --cmd-log."
      ]

commandDisplayParser :: Parser (Maybe KeyHide)
commandDisplayParser =
  OA.optional
    $ OA.flag'
      KeyHideOn
      ( mconcat
          [ OA.short 'k',
            OA.long "key-hide",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "By default, we display the key name from the legend over the ",
          "actual command that was run, if the former exists. This flag ",
          "instead shows the literal command. Commands without keys are ",
          "unaffected."
        ]

noKeyHideParser :: Parser Bool
noKeyHideParser =
  OA.switch
    $ mconcat
      [ OA.long "no-key-hide",
        OA.hidden,
        mkHelp "Disables --key-hide."
      ]

pollIntervalParser :: Parser (Maybe PollInterval)
pollIntervalParser =
  OA.optional
    $ OA.option
      readPI
      ( mconcat
          [ OA.long "poll-interval",
            OA.short 'p',
            mkHelp helpTxt,
            OA.metavar "NATURAL"
          ]
      )
  where
    readPI = do
      s <- OA.str
      case TR.readMaybe s of
        Nothing ->
          OA.readerAbort
            $ ErrorMsg
            $ "Could not parse poll-interval: "
            <> s
        Just n -> pure $ MkPollInterval n
    helpTxt =
      mconcat
        [ "Non-negative integer used in conjunction with --cmd-log and ",
          "--file-log that determines how quickly we poll commands for ",
          "logs, in microseconds. A value of 0 is interpreted as infinite ",
          "i.e. limited only by the CPU. Defaults to ",
          prettyPollInterval defaultPollInterval,
          ". Note that lower values will increase CPU usage. In particular, ",
          "0 will max out a CPU thread."
        ]

    prettyPollInterval =
      unpack
        . T.reverse
        . T.intercalate ","
        . T.chunksOf 3
        . T.reverse
        . showt
        . view #unPollInterval

noPollIntervalParser :: Parser Bool
noPollIntervalParser =
  OA.switch
    $ mconcat
      [ OA.long "no-poll-interval",
        OA.hidden,
        mkHelp "Disables --poll-interval."
      ]

timerFormatParser :: Parser (Maybe TimerFormat)
timerFormatParser =
  OA.optional
    $ OA.option readTimerFormat
    $ mconcat
      [ OA.long "timer-format",
        mkHelp helpTxt,
        OA.metavar TimerFormat.timerFormatStr
      ]
  where
    readTimerFormat = OA.str >>= TimerFormat.parseTimerFormat
    helpTxt =
      mconcat
        [ "How to format the timer. Defaults to prose_compact e.g. ",
          "'2 hours, 3 seconds'."
        ]

noTimerFormatParser :: Parser Bool
noTimerFormatParser =
  OA.switch
    $ mconcat
      [ OA.long "no-timer-format",
        OA.hidden,
        mkHelp "Disables --timer-format."
      ]

initParser :: Parser (Maybe Text)
initParser =
  OA.optional
    $ OA.option OA.str
    $ mconcat
      [ OA.long "init",
        OA.short 'i',
        mkHelp helpTxt,
        OA.metavar "STRING"
      ]
  where
    helpTxt =
      mconcat
        [ "If given, init is run before each command. That is, ",
          "'shrun --init \". ~/.bashrc\" foo bar' is equivalent ",
          "to 'shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"'."
        ]

noInitParser :: Parser Bool
noInitParser =
  OA.switch
    $ mconcat
      [ OA.long "no-init",
        OA.hidden,
        mkHelp "Disables --init."
      ]

notifySystemParser :: Parser (Maybe NotifySystemP1)
notifySystemParser =
  OA.optional
    $ OA.option readNotifySystem
    $ mconcat
      [ OA.long "notify-system",
        mkHelp helpTxt,
        OA.metavar Notify.notifySystemStr
      ]
  where
    readNotifySystem = OA.str >>= Notify.parseNotifySystem
    helpTxt =
      mconcat
        [ "The system used for sending notifications. 'dbus' and 'notify-send' ",
          "available on linux, whereas 'apple-script' is available for osx."
        ]

noNotifySystemParser :: Parser Bool
noNotifySystemParser =
  OA.switch
    $ mconcat
      [ OA.long "no-notify-system",
        OA.hidden,
        mkHelp "Disables --notify-system."
      ]

notifyActionParser :: Parser (Maybe NotifyAction)
notifyActionParser =
  OA.optional
    $ OA.option readNotifyAction
    $ mconcat
      [ OA.long "notify-action",
        mkHelp helpTxt,
        OA.metavar Notify.notifyActionStr
      ]
  where
    readNotifyAction = OA.str >>= Notify.parseNotifyAction
    helpTxt =
      mconcat
        [ "Sends notifications for various actions. 'Final' sends off a ",
          "notification when Shrun itself finishes whereas 'command' (which ",
          "implies 'final') sends off one each time a command finishes."
        ]

noNotifyActionParser :: Parser Bool
noNotifyActionParser =
  OA.switch
    $ mconcat
      [ OA.long "no-notify-action",
        OA.hidden,
        mkHelp "Disables --notify-action."
      ]

notifyTimeoutParser :: Parser (Maybe NotifyTimeout)
notifyTimeoutParser =
  OA.optional
    $ OA.option readNotifySystem
    $ mconcat
      [ OA.long "notify-timeout",
        mkHelp helpTxt,
        OA.metavar Notify.notifyTimeoutStr
      ]
  where
    readNotifySystem = OA.str >>= Notify.parseNotifyTimeout
    helpTxt = "When to timeout success notifications. Defaults to 10 seconds."

noNotifyTimeoutParser :: Parser Bool
noNotifyTimeoutParser =
  OA.switch
    $ mconcat
      [ OA.long "no-notify-timeout",
        OA.hidden,
        mkHelp "Disables --notify-timeout."
      ]

commandsParser :: Parser (NESeq Text)
commandsParser =
  U.unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )

-- Looks a bit convoluted, but this gets us what we want:
-- 1. lines aligned (paragraph)
-- 2. linebreak at the end (fmap hardline)
mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
