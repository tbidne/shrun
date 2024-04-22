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
        cmdLogReadSize,
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
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
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
  cmdLogReadSize <- cmdLogReadSizeParser
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
        cmdLogReadSize,
        timerFormat,
        cmdNameTrunc,
        cmdLogging,
        fileLogging,
        notify
      }

timeoutParser :: Parser (WithDisabled Timeout)
timeoutParser = Utils.withDisabledParser mainParser "timeout"
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

initParser :: Parser (WithDisabled Text)
initParser = Utils.withDisabledParser mainParser "init"
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

keyHideParser :: Parser (WithDisabled KeyHide)
keyHideParser = Utils.withDisabledParser mainParser "key-hide"
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

pollIntervalParser :: Parser (WithDisabled PollInterval)
pollIntervalParser = Utils.withDisabledParser mainParser "poll-interval"
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

cmdLogReadSizeParser :: Parser (WithDisabled (Bytes B Natural))
cmdLogReadSizeParser = Utils.withDisabledParser mainParser "cmd-log-read-size"
  where
    mainParser =
      OA.optional
        $ OA.option
          readcmdLogReadSize
          ( mconcat
              [ OA.long "cmd-log-read-size",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    readcmdLogReadSize = MkBytes <$> OA.auto
    helpTxt =
      mconcat
        [ "Non-negative integer that determines that max number of bytes in ",
          "a single read when streaming command logs (--cmd-log and ",
          "--file-log). Logs larger than --cmd-log-read-size will be read in ",
          "a subsequent read, hence broken across lines. The default is 1024."
        ]

timerFormatParser :: Parser (WithDisabled TimerFormat)
timerFormatParser = Utils.withDisabledParser mainParser "timer-format"
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

cmdNameTruncParser :: Parser (WithDisabled (Truncation TCmdName))
cmdNameTruncParser = Utils.withDisabledParser mainParser "cmd-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation OA.auto)
          ( mconcat
              [ OA.long "cmd-name-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation."
        ]
