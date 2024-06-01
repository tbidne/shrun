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
  )
import Shrun.Configuration.Data.CommandLogging qualified as CommandLogging
import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.PollInterval qualified as PollInterval
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize)
import Shrun.Configuration.Data.CommandLogging.ReadSize qualified as ReadSize
import Shrun.Configuration.Data.CommandLogging.ReadStrategy (ReadStrategy)
import Shrun.Configuration.Data.CommandLogging.ReadStrategy qualified as ReadStrategy
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
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

bufferLengthParser :: Parser (WithDisabled BufferLength)
bufferLengthParser = Utils.withDisabledParser mainParser "command-log-buffer-length"
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

bufferTimeoutParser :: Parser (WithDisabled BufferTimeout)
bufferTimeoutParser = Utils.withDisabledParser mainParser "command-log-buffer-timeout"
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

pollIntervalParser :: Parser (WithDisabled PollInterval)
pollIntervalParser = Utils.withDisabledParser mainParser "command-log-poll-interval"
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

readSizeParser :: Parser (WithDisabled ReadSize)
readSizeParser = Utils.withDisabledParser mainParser "command-log-read-size"
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

readStrategyParser :: Parser (WithDisabled ReadStrategy)
readStrategyParser = Utils.withDisabledParser mainParser "command-log-read-strategy"
  where
    mainParser =
      OA.optional
        $ OA.option (ReadStrategy.parseReadStrategy OA.str)
        $ mconcat
          [ OA.long "command-log-read-strategy",
            Utils.mkHelp helpTxt,
            OA.metavar ReadStrategy.readStrategyStr
          ]
    helpTxt =
      mconcat
        [ "By default (strategy = block), we read --command-log-read-size ",
          "number of bytes at a time. The block-line-buffer strategy will ",
          "buffer logs until a newline is found, or some size threshold is ",
          "crossed. This can help preserve formatting in the file logs. This ",
          "should only be used when running a _single_ command."
        ]

reportReadErrorsParser :: Parser (WithDisabled ())
reportReadErrorsParser =
  Utils.withDisabledParserOpts
    OA.internal
    mainParser
    "command-log-report-read-errors"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "command-log-report-read-errors",
              OA.internal,
              Utils.mkHelp helpTxt
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing
    helpTxt = "If active, logs read errors when streaming commands."
