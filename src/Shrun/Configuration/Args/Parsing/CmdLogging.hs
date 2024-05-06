-- | CLI parsing for CmdLoggingArgs
module Shrun.Configuration.Args.Parsing.CmdLogging
  ( cmdLoggingParser,
  )
where

import Data.Text qualified as T
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingArgs,
    CmdLoggingP
      ( MkCmdLoggingP,
        pollInterval,
        readSize
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Configuration.Default (Default (def))
import Shrun.Data.CmdLogReadSize (CmdLogReadSize (MkCmdLogReadSize))
import Shrun.Data.PollInterval (PollInterval)
import Shrun.Data.PollInterval qualified as PollInterval
import Shrun.Prelude

cmdLoggingParser :: Parser CmdLoggingArgs
cmdLoggingParser = do
  pollInterval <- pollIntervalParser
  readSize <- readSizeParser

  pure
    $ MkCmdLoggingP
      { pollInterval,
        readSize
      }

pollIntervalParser :: Parser (WithDisabled PollInterval)
pollIntervalParser = Utils.withDisabledParser mainParser "cmd-log-poll-interval"
  where
    mainParser =
      OA.optional
        $ OA.option
          (PollInterval.parsePollInterval OA.auto)
          ( mconcat
              [ OA.long "cmd-log-poll-interval",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer used in conjunction with --console-log-cmd and ",
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

readSizeParser :: Parser (WithDisabled CmdLogReadSize)
readSizeParser = Utils.withDisabledParser mainParser "cmd-log-read-size"
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
    readcmdLogReadSize = MkCmdLogReadSize . MkBytes <$> OA.auto
    helpTxt =
      mconcat
        [ "Non-negative integer that determines that max number of bytes in ",
          "a single read when streaming command logs (--console-log-cmd and ",
          "--file-log). Logs larger than --cmd-log-read-size will be read in ",
          "a subsequent read, hence broken across lines. The default is 1024."
        ]
