-- | CLI parsing for CoreConfigArgs
module Shrun.Configuration.Args.Parsing.Core
  ( coreParser,
  )
where

import Data.Text qualified as T
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.CmdLogging qualified as CmdLogging
import Shrun.Configuration.Args.Parsing.FileLogging qualified as FileLogging
import Shrun.Configuration.Args.Parsing.Notify qualified as Notify
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
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
import Shrun.Configuration.Data.WithDisable (WithDisable)
import Shrun.Data.KeyHide (KeyHide (KeyHideOn))
import Shrun.Data.PollInterval
  ( PollInterval,
    defaultPollInterval,
  )
import Shrun.Data.PollInterval qualified as PollInterval
import Shrun.Data.Timeout (Timeout)
import Shrun.Data.Timeout qualified as Timeout
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.TimerFormat qualified as TimerFormat
import Shrun.Data.Truncation (TruncRegion (TCmdName), Truncation)
import Shrun.Data.Truncation qualified as Trunc
import Shrun.Prelude

coreParser :: Parser CoreConfigArgs
coreParser = do
  timeout <- timeoutParser
  init <- initParser
  keyHide <- keyHideParser
  pollInterval <- pollIntervalParser
  cmdLogSize <- cmdLogSizeParser
  timerFormat <- timerFormatParser
  cmdNameTrunc <- cmdNameTruncParser
  cmdLogging <- CmdLogging.cmdLoggingParser
  fileLogging <- FileLogging.fileLoggingParser
  notify <- Notify.notifyParser

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

timeoutParser :: Parser (WithDisable (Maybe Timeout))
timeoutParser = Utils.withDisableParser mainParser "timeout"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Timeout.parseTimeout OA.auto OA.str)
          ( mconcat
              [ OA.long "timeout",
                OA.short 't',
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | STRING)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

initParser :: Parser (WithDisable (Maybe Text))
initParser = Utils.withDisableParser mainParser "init"
  where
    mainParser =
      OA.optional
        $ OA.option OA.str
        $ mconcat
          [ OA.long "init",
            OA.short 'i',
            Utils.mkHelp helpTxt,
            OA.metavar "STRING"
          ]
    helpTxt =
      mconcat
        [ "If given, init is run before each command. That is, ",
          "'shrun --init \". ~/.bashrc\" foo bar' is equivalent ",
          "to 'shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"'."
        ]

keyHideParser :: Parser (WithDisable (Maybe KeyHide))
keyHideParser = Utils.withDisableParser mainParser "key-hide"
  where
    mainParser =
      OA.optional
        $ OA.flag'
          KeyHideOn
          ( mconcat
              [ OA.short 'k',
                OA.long "key-hide",
                Utils.mkHelp helpTxt
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
pollIntervalParser = Utils.withDisableParser mainParser "poll-interval"
  where
    mainParser =
      OA.optional
        $ OA.option
          (PollInterval.parsePollInterval OA.auto)
          ( mconcat
              [ OA.long "poll-interval",
                OA.short 'p',
                Utils.mkHelp helpTxt,
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

cmdLogSizeParser :: Parser (WithDisable (Maybe (Bytes B Natural)))
cmdLogSizeParser = Utils.withDisableParser mainParser "cmd-log-size"
  where
    mainParser =
      OA.optional
        $ OA.option
          readCmdLogSize
          ( mconcat
              [ OA.long "cmd-log-size",
                Utils.mkHelp helpTxt,
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

timerFormatParser :: Parser (WithDisable (Maybe TimerFormat))
timerFormatParser = Utils.withDisableParser mainParser "timer-format"
  where
    mainParser =
      OA.optional
        $ OA.option (TimerFormat.parseTimerFormat OA.str)
        $ mconcat
          [ OA.long "timer-format",
            Utils.mkHelp helpTxt,
            OA.metavar TimerFormat.timerFormatStr
          ]
    helpTxt =
      mconcat
        [ "How to format the timer. Defaults to prose_compact e.g. ",
          "'2 hours, 3 seconds'."
        ]

cmdNameTruncParser :: Parser (WithDisable (Maybe (Truncation TCmdName)))
cmdNameTruncParser = Utils.withDisableParser mainParser "cmd-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation OA.auto)
          ( mconcat
              [ OA.long "cmd-name-trunc",
                OA.short 'x',
                Utils.mkHelp helpTxt,
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
