{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
-- For TH, for some reason

-- | Provides functionality for parsing command line arguments.
module Shrun.Configuration.Args.Parsing
  ( Args (..),
    parserInfoArgs,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Version (Version (versionBranch))
import Effects.Optparse (validOsPath)
import Options.Applicative
  ( Parser,
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
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_shrun qualified as Paths
import Shrun.Configuration.Args.TH (getDefaultConfigTH)
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingArgs,
    CmdLoggingP (MkCmdLoggingP, lineTrunc, stripControl),
  )
import Shrun.Configuration.Data.ConfigPhase (WithDisable (MkWithDisable))
import Shrun.Configuration.Data.Core
  ( CoreConfigArgs,
    CoreConfigP
      ( MkCoreConfigP,
        cmdLogSize,
        cmdLogging,
        cmdNameTrunc,
        fileLogging,
        init,
        keyHide,
        notify,
        pollInterval,
        timeout,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.FileLogging
  ( FileLoggingArgs,
    FileLoggingP (MkFileLoggingP, mode, path, sizeMode, stripControl),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyArgs,
    NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Data.FileMode (FileMode)
import Shrun.Data.FileMode qualified as FileMode
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FilePathDefault qualified as FilePathDefault
import Shrun.Data.FileSizeMode (FileSizeMode)
import Shrun.Data.FileSizeMode qualified as FileSizeMode
import Shrun.Data.KeyHide (KeyHide (KeyHideOn))
import Shrun.Data.PollInterval (PollInterval, defaultPollInterval)
import Shrun.Data.PollInterval qualified as PollInterval
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.StripControl qualified as StripControl
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.Timeout qualified as Timeout
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.TimerFormat qualified as TimerFormat
import Shrun.Data.Truncation
  ( LineTruncation,
    TruncRegion (TCmdName),
    Truncation,
  )
import Shrun.Data.Truncation qualified as Trunc
import Shrun.Notify.Types
  ( NotifyAction,
    NotifySystemP1,
    NotifyTimeout,
  )
import Shrun.Notify.Types qualified as Notify
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | CLI args.
data Args = MkArgs
  { -- | Optional config file.
    configPath :: WithDisable (Maybe OsPath),
    -- | Whether to log commands.
    cmdLog :: WithDisable Bool,
    -- | Core config.
    coreConfig :: CoreConfigArgs,
    -- | List of commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Args

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
argsParser = do
  MkArgs
    <$> configParser
    <*> cmdLogParser
    <*> coreParser
    <**> defaultConfig
    <**> version
    <**> OA.helper
    <*> commandsParser
  where
    coreParser :: Parser CoreConfigArgs
    coreParser = do
      timeout <- timeoutParser
      init <- initParser
      keyHide <- keyHideParser
      pollInterval <- pollIntervalParser
      cmdLogSize <- cmdLogSizeParser
      timerFormat <- timerFormatParser
      cmdNameTrunc <- cmdNameTruncParser
      cmdLogging <- cmdLoggingParser
      fileLogging <- fileLoggingParser
      notify <- notifyParser

      pure
        $ MkCoreConfigP
          { timeout,
            init,
            keyHide,
            pollInterval,
            cmdLogSize,
            timerFormat,
            cmdNameTrunc,
            cmdLogging,
            fileLogging,
            notify
          }

    cmdLoggingParser :: Parser CmdLoggingArgs
    cmdLoggingParser = do
      stripControl <- cmdLogStripControlParser
      lineTrunc <- cmdLogLineTruncParser

      pure
        $ MkCmdLoggingP
          { stripControl,
            lineTrunc
          }

    fileLoggingParser :: Parser FileLoggingArgs
    fileLoggingParser = do
      path <- fileLogParser
      stripControl <- fileLogStripControlParser
      mode <- fileLogModeParser
      sizeMode <- fileLogSizeModeParser

      pure
        $ MkFileLoggingP
          { path,
            stripControl,
            mode,
            sizeMode
          }

    notifyParser :: Parser NotifyArgs
    notifyParser = do
      action <- notifyActionParser
      system <- notifySystemParser
      timeout <- notifyTimeoutParser

      pure
        $ MkNotifyP
          { action,
            system,
            timeout
          }

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

defaultConfig :: Parser (a -> a)
defaultConfig = OA.infoOption (unpack txt) (OA.long "default-config" <> mkHelp help)
  where
    txt = T.unlines $$getDefaultConfigTH
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (WithDisable (Maybe OsPath))
configParser = withDisableParserHelp mainParser "config" noHelpTxt
  where
    mainParser =
      OA.optional
        $ OA.option
          validOsPath
          ( mconcat
              [ OA.long "config",
                OA.short 'c',
                mkHelp mainHelpTxt,
                OA.metavar "PATH"
              ]
          )
    mainHelpTxt =
      mconcat
        [ "Path to TOML config file. If this argument is not given ",
          "we automatically look in the XDG config directory ",
          "e.g. ~/.config/shrun/config.toml"
        ]
    noHelpTxt =
      mconcat
        [ "Overrides toml file config regardless of how it was obtained i.e. ",
          "explicit --config or implicit reading of the XDG config file. ",
          "Used for when a config file exists at the expected XDG ",
          "location, but we want to ignore it."
        ]

timeoutParser :: Parser (WithDisable (Maybe Timeout))
timeoutParser = withDisableParser mainParser "timeout"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Timeout.parseTimeout OA.auto OA.str)
          ( mconcat
              [ OA.long "timeout",
                OA.short 't',
                mkHelp helpTxt,
                OA.metavar "(NATURAL | STRING)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

cmdNameTruncParser :: Parser (WithDisable (Maybe (Truncation TCmdName)))
cmdNameTruncParser = withDisableParser mainParser "cmd-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation OA.auto)
          ( mconcat
              [ OA.long "cmd-name-trunc",
                OA.short 'x',
                mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation. This affects ",
          "everywhere the command/key-name shows up (i.e. in command logs or ",
          "final success/error message); File logs created via --file-log ",
          "are unaffected."
        ]

cmdLogSizeParser :: Parser (WithDisable (Maybe (Bytes B Natural)))
cmdLogSizeParser = withDisableParser mainParser "cmd-log-size"
  where
    mainParser =
      OA.optional
        $ OA.option
          readCmdLogSize
          ( mconcat
              [ OA.long "cmd-log-size",
                mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    readCmdLogSize = MkBytes <$> OA.auto
    helpTxt =
      mconcat
        [ "Non-negative integer that determines the size (bytes) of command ",
          "logs in a single read (--cmd-log and --file-log). Logs larger than ",
          "--cmd-log-size will be read in a subsequent read, hence broken ",
          "across lines. The default is 1024."
        ]

cmdLogLineTruncParser :: Parser (WithDisable (Maybe LineTruncation))
cmdLogLineTruncParser = withDisableParser mainParser "cmd-log-line-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseLineTruncation OA.auto OA.str)
          ( mconcat
              [ OA.long "cmd-log-line-trunc",
                OA.short 'y',
                mkHelp helpTxt,
                OA.metavar "(NATURAL | detect)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of logs ",
          "produced via --cmd-log in the console logs. Can also be the ",
          "string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation. This does ",
          "not affect file logs with --file-log."
        ]

cmdLogStripControlParser :: Parser (WithDisable (Maybe StripControl))
cmdLogStripControlParser = withDisableParser mainParser "cmd-log-strip-control"
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "cmd-log-strip-control",
                OA.short 's',
                mkHelp helpTxt,
                OA.metavar "(all | smart | none)"
              ]
          )
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

fileLogParser :: Parser (WithDisable (Maybe FilePathDefault))
fileLogParser = withDisableParser mainParser "file-log"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FilePathDefault.parseFilePathDefault OA.str)
          ( mconcat
              [ OA.long "file-log",
                OA.short 'f',
                mkHelp helpTxt,
                OA.metavar "(default | PATH)"
              ]
          )
    helpTxt =
      mconcat
        [ "If a path is supplied, all logs will additionally be written to ",
          "the supplied file. Furthermore, command logs will be written to ",
          "the file irrespective of --cmd-log. Console logging is unaffected. ",
          "This can be useful for investigating command failures. ",
          "If the string 'default' is given, then we write to the XDG config ",
          "directory e.g. ~/.config/shrun/shrun.log."
        ]

fileLogModeParser :: Parser (WithDisable (Maybe FileMode))
fileLogModeParser = withDisableParser mainParser "file-log-mode"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileMode.parseFileMode OA.str)
          ( mconcat
              [ OA.long "file-log-mode",
                mkHelp helpTxt,
                OA.metavar "(append | write)"
              ]
          )
    helpTxt = "Mode in which to open the log file. Defaults to write."

fileLogStripControlParser :: Parser (WithDisable (Maybe StripControl))
fileLogStripControlParser = withDisableParser mainParser "file-log-strip-control"
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "file-log-strip-control",
                mkHelp helpTxt,
                OA.metavar "(all | smart | none)"
              ]
          )
    helpTxt =
      mconcat
        [ "--cmd-log-strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

fileLogSizeModeParser :: Parser (WithDisable (Maybe FileSizeMode))
fileLogSizeModeParser = withDisableParser mainParser "file-log-size-mode"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileSizeMode.parseFileSizeMode OA.str)
          ( mconcat
              [ OA.long "file-log-size-mode",
                mkHelp helpTxt,
                OA.metavar FileSizeMode.expectedStr
              ]
          )
    helpTxt =
      mconcat
        [ "Sets a threshold for the file log size, upon which we either ",
          "print a warning or delete the file, if it is exceeded. ",
          "The SIZE should include the value and units e.g. ",
          "warn 10 mb, warn 5 gigabytes, delete 20.5B. Defaults to warning ",
          "at 50 mb."
        ]

cmdLogParser :: Parser (WithDisable Bool)
cmdLogParser = withDisableParser mainParser "cmd-log"
  where
    mainParser =
      OA.switch
        ( mconcat
            [ OA.short 'l',
              OA.long "cmd-log",
              mkHelp helpTxt
            ]
        )
    helpTxt =
      mconcat
        [ "The default behavior is to swallow logs for the commands ",
          "themselves. This flag gives each command a console region in ",
          "which its logs will be printed. Only the latest log per region ",
          "is show at a given time."
        ]

keyHideParser :: Parser (WithDisable (Maybe KeyHide))
keyHideParser = withDisableParser mainParser "key-hide"
  where
    mainParser =
      OA.optional
        $ OA.flag'
          KeyHideOn
          ( mconcat
              [ OA.short 'k',
                OA.long "key-hide",
                mkHelp helpTxt
              ]
          )
    helpTxt =
      mconcat
        [ "By default, we display the key name from the legend over the ",
          "actual command that was run, if the former exists. This flag ",
          "instead shows the literal command. Commands without keys are ",
          "unaffected."
        ]

pollIntervalParser :: Parser (WithDisable (Maybe PollInterval))
pollIntervalParser = withDisableParser mainParser "poll-interval"
  where
    mainParser =
      OA.optional
        $ OA.option
          (PollInterval.parsePollInterval OA.auto)
          ( mconcat
              [ OA.long "poll-interval",
                OA.short 'p',
                mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
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

timerFormatParser :: Parser (WithDisable (Maybe TimerFormat))
timerFormatParser = withDisableParser mainParser "timer-format"
  where
    mainParser =
      OA.optional
        $ OA.option (TimerFormat.parseTimerFormat OA.str)
        $ mconcat
          [ OA.long "timer-format",
            mkHelp helpTxt,
            OA.metavar TimerFormat.timerFormatStr
          ]
    helpTxt =
      mconcat
        [ "How to format the timer. Defaults to prose_compact e.g. ",
          "'2 hours, 3 seconds'."
        ]

initParser :: Parser (WithDisable (Maybe Text))
initParser = withDisableParser mainParser "init"
  where
    mainParser =
      OA.optional
        $ OA.option OA.str
        $ mconcat
          [ OA.long "init",
            OA.short 'i',
            mkHelp helpTxt,
            OA.metavar "STRING"
          ]
    helpTxt =
      mconcat
        [ "If given, init is run before each command. That is, ",
          "'shrun --init \". ~/.bashrc\" foo bar' is equivalent ",
          "to 'shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"'."
        ]

notifySystemParser :: Parser (WithDisable (Maybe NotifySystemP1))
notifySystemParser = withDisableParser mainParser "notify-system"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifySystem OA.str)
        $ mconcat
          [ OA.long "notify-system",
            mkHelp helpTxt,
            OA.metavar Notify.notifySystemStr
          ]
    helpTxt =
      mconcat
        [ "The system used for sending notifications. 'dbus' and 'notify-send' ",
          "available on linux, whereas 'apple-script' is available for osx."
        ]

notifyActionParser :: Parser (WithDisable (Maybe NotifyAction))
notifyActionParser = withDisableParser mainParser "notify-action"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifyAction OA.str)
        $ mconcat
          [ OA.long "notify-action",
            mkHelp helpTxt,
            OA.metavar Notify.notifyActionStr
          ]
    helpTxt =
      mconcat
        [ "Sends notifications for various actions. 'Final' sends off a ",
          "notification when Shrun itself finishes whereas 'command' sends ",
          "off one each time a command finishes. 'All' implies 'final' and ",
          "'command'."
        ]

notifyTimeoutParser :: Parser (WithDisable (Maybe NotifyTimeout))
notifyTimeoutParser = withDisableParser mainParser "notify-timeout"
  where
    mainParser =
      OA.optional
        $ OA.option (Notify.parseNotifyTimeout OA.str)
        $ mconcat
          [ OA.long "notify-timeout",
            mkHelp helpTxt,
            OA.metavar Notify.notifyTimeoutStr
          ]
    helpTxt = "When to timeout success notifications. Defaults to 10 seconds."

commandsParser :: Parser (NESeq Text)
commandsParser =
  U.unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )

withDisableParser :: Parser a -> String -> Parser (WithDisable a)
withDisableParser mainParser name =
  withDisableParserHelp mainParser name ("Disables --" ++ name)

withDisableParserHelp :: Parser a -> String -> String -> Parser (WithDisable a)
withDisableParserHelp mainParser name helpTxt = do
  x <- mainParser
  y <- noParser
  pure $ MkWithDisable (x, y)
  where
    noParser =
      OA.flag
        False
        True
        ( mconcat
            [ OA.long $ "no-" ++ name,
              OA.hidden,
              mkHelp helpTxt
            ]
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
