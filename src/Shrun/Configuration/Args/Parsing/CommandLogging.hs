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
  ( CommandLoggingArgs,
    CommandLoggingP
      ( MkCommandLoggingP,
        pollInterval,
        readSize
      ),
  )
import Shrun.Configuration.Data.CommandLogging.PollInterval (PollInterval)
import Shrun.Configuration.Data.CommandLogging.PollInterval qualified as PollInterval
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize)
import Shrun.Configuration.Data.CommandLogging.ReadSize qualified as ReadSize
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

commandLoggingParser :: Parser CommandLoggingArgs
commandLoggingParser = do
  pollInterval <- pollIntervalParser
  readSize <- readSizeParser

  pure
    $ MkCommandLoggingP
      { pollInterval,
        readSize
      }

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
          "broken across lines. The default is '1 kb'."
        ]
