-- | CLI parsing for CommandLoggingArgs
module Shrun.Configuration.Args.Parsing.CommandLogging
  ( commandLoggingParser,
  )
where

import Data.Text qualified as T
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.CommandLogging
  ( BufferLength,
    BufferTimeout (MkBufferTimeout),
    CommandLoggingArgs,
    CommandLoggingP
      ( MkCommandLoggingP,
        bufferLength,
        pollInterval,
        readSize,
        readStrategy,
        reportReadErrors
      ),
    ReportReadErrorsSwitch (MkReportReadErrorsSwitch),
  )
import Shrun.Configuration.Data.CommandLogging qualified as CommandLogging
import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.PollInterval qualified as PollInterval
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize)
import Shrun.Configuration.Data.CommandLogging.ReadSize qualified as ReadSize
import Shrun.Configuration.Data.CommandLogging.ReadStrategy (ReadStrategy)
import Shrun.Configuration.Data.CommandLogging.ReadStrategy qualified as ReadStrategy
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

commandLoggingParser :: Parser CommandLoggingArgs
commandLoggingParser = do
  bufferLength <- bufferLengthParser
  bufferTimeout <- bufferTimeoutParser
  pollInterval <- pollIntervalParser
  readSize <- readSizeParser
  readStrategy <- readStrategyParser
  reportReadErrors <- reportReadErrorsParser

  pure
    $ MkCommandLoggingP
      { bufferLength,
        bufferTimeout,
        pollInterval,
        readSize,
        readStrategy,
        reportReadErrors
      }

bufferLengthParser :: Parser (Maybe BufferLength)
bufferLengthParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (CommandLogging.parseBufferLength Utils.autoStripUnderscores)
          ( mconcat
              [ OA.long "command-log-buffer-length",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Max text length held by the log buffer, used in conjunction ",
          "with --command-log-read-strategy block-line-buffer. Defaults to ",
          "1,000 characters."
        ]

bufferTimeoutParser :: Parser (Maybe BufferTimeout)
bufferTimeoutParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (MkBufferTimeout <$> Timeout.parseTimeout Utils.autoStripUnderscores OA.str)
          ( mconcat
              [ OA.long "command-log-buffer-timeout",
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | STRING)"
              ]
          )
    helpTxt =
      mconcat
        [ "Max time the log buffer will hold a log before flushing it, used ",
          "in conjunction with --command-log-read-strategy ",
          "block-line-buffer. Defaults to 30 seconds."
        ]

pollIntervalParser :: Parser (Maybe PollInterval)
pollIntervalParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (PollInterval.parsePollInterval Utils.autoStripUnderscores)
          ( mconcat
              [ OA.long "command-log-poll-interval",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer used in conjunction with --console-log-command and ",
          "--file-log that determines how quickly we poll commands for ",
          "logs, in microseconds. A value of 0 is interpreted as infinite ",
          "i.e. limited only by the CPU. Defaults to ",
          prettyPollInterval def,
          ". Note that lower values will increase CPU usage. In particular, ",
          "0 will max out a CPU thread."
        ]

    prettyPollInterval :: PollInterval -> String
    prettyPollInterval =
      unpack
        . T.reverse
        . T.intercalate ","
        . T.chunksOf 3
        . T.reverse
        . showt
        . view #unPollInterval

readSizeParser :: Parser (Maybe ReadSize)
readSizeParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (ReadSize.parseReadSize OA.str)
          ( mconcat
              [ OA.long "command-log-read-size",
                Utils.mkHelp helpTxt,
                OA.metavar "BYTES"
              ]
          )

    helpTxt =
      mconcat
        [ "The max number of bytes in a single read when streaming command ",
          "logs (--console-log-command and --file-log). Logs larger than ",
          "--command-log-read-size will be read in a subsequent read, hence ",
          "broken across lines. The default is '16 kb'."
        ]

readStrategyParser :: Parser (Maybe ReadStrategy)
readStrategyParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option (ReadStrategy.parseReadStrategy OA.str)
        $ mconcat
          [ OA.long "command-log-read-strategy",
            Utils.mkHelpNoLine helpTxt,
            OA.metavar ReadStrategy.readStrategyStr
          ]
    helpTxt =
      mconcat
        [ "The 'block' strategy reads N (--command-log-read-size) bytes at a time, ",
          "whereas 'block-line-buffer' also reads N bytes at a time, but buffers ",
          "newlines, for potentially nicer formatted logs. By default, we use ",
          "'block-line-buffer' if we are only running one command and/or file-logging ",
          "is not active. For multiple commands with file-logging, we default to ",
          "'block'. This option explicitly sets the strategy."
        ]

reportReadErrorsParser :: Parser (Maybe ReportReadErrorsSwitch)
reportReadErrorsParser =
  Utils.switchParserOpts
    OA.internal
    MkReportReadErrorsSwitch
    "command-log-report-read-errors"
    helpTxt
  where
    helpTxt = "If active, logs read errors when streaming commands."
