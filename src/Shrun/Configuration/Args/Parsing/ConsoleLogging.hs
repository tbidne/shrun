{-# LANGUAGE OverloadedLists #-}

-- | CLI parsing for ConsoleLoggingArgs
module Shrun.Configuration.Args.Parsing.ConsoleLogging
  ( consoleLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (MkConsoleLogCmdSwitch),
    ConsoleLoggingArgs,
    ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat)
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat qualified as TimerFormat
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.StripControl qualified as StripControl
import Shrun.Configuration.Data.Truncation
  ( LineTruncation,
    TruncRegion (TruncCommandName),
    Truncation,
  )
import Shrun.Configuration.Data.Truncation qualified as Trunc
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

consoleLoggingParser :: Parser ConsoleLoggingArgs
consoleLoggingParser = do
  commandLogging <- commandLoggingParser
  commandNameTrunc <- commandNameTruncParser
  lineTrunc <- lineTruncParser
  stripControl <- stripControlParser
  timerFormat <- timerFormatParser

  pure
    $ MkConsoleLoggingP
      { commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      }

commandLoggingParser :: Parser (Maybe ConsoleLogCmdSwitch)
commandLoggingParser = Utils.switchParser MkConsoleLogCmdSwitch "console-log-command" helpTxt
  where
    helpTxt =
      mconcat
        [ "This flag gives each command a console region in which its logs ",
          "will be printed, as opposed to swallowing command logs. Only the ",
          "latest log per region is shown at a given time. Defaults to 'on'."
        ]

commandNameTruncParser :: Parser (Maybe (WithDisabled (Truncation TruncCommandName)))
commandNameTruncParser =
  Utils.mWithDisabledParser
    (Trunc.parseTruncation Utils.autoStripUnderscores)
    opts
    "NATURAL"
  where
    opts =
      [ OA.long "console-log-command-name-trunc",
        Utils.mkHelp helpTxt
      ]
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation."
        ]

lineTruncParser :: Parser (Maybe (WithDisabled LineTruncation))
lineTruncParser =
  Utils.mWithDisabledParser
    (Trunc.parseLineTruncation Utils.autoStripUnderscores OA.str)
    opts
    Trunc.lineTruncStr
  where
    opts =
      [ OA.long "console-log-line-trunc",
        OA.completeWith ["detect"],
        Utils.mkHelp helpTxt
      ]
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of console logs. Can ",
          "also be the string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to 'detect' if --console-log-command is on. ",
          "Note that \"log prefixes\" (e.g. labels like [Success], timestamps) ",
          "are counted towards the total length but are never truncated."
        ]

stripControlParser :: Parser (Maybe ConsoleLogStripControl)
stripControlParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "console-log-strip-control",
                OA.completeWith ["all", "smart", "off"],
                helpTxt,
                OA.metavar StripControl.stripControlStr
              ]
          )

    helpTxt =
      Utils.itemize
        $ intro
        :<|| [ hall,
               hsmart
             ]

    intro =
      mconcat
        [ "Control characters can wreak layout havoc, hence this option for ",
          "stripping such characters."
        ]

    hall = "all: Strips all such chars."
    hsmart =
      mconcat
        [ "smart: The default. Attempts to strip only the control chars that ",
          "affect layout (e.g. cursor movements) and leaves others unaffected ",
          "(e.g. colors). This has the potential to be the 'prettiest' though ",
          "it is possible to miss some chars."
        ]

timerFormatParser :: Parser (Maybe TimerFormat)
timerFormatParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option (TimerFormat.parseTimerFormat OA.str)
        $ mconcat
          [ OA.long "console-log-timer-format",
            OA.completeWith ["digital_compact", "digital_full", "prose_compact", "prose_full"],
            OA.metavar "TIME_FMT",
            helpTxt
          ]
    helpTxt =
      Utils.itemizeNoLine
        $ intro
        :<|| [ hdc,
               hdf,
               hpc,
               hpf
             ]

    intro = "How to format the timer. Options:"

    hdc = "digital_compact: e.g. '02:00:03'."
    hdf = "digital_full: e.g. '00:02:00:03'."
    hpc = "prose_compact: The default e.g. '2 hours, 3 seconds'."
    hpf = "prose_full: e.g. '0 days, 2 hours, 0 minutes, 3 seconds'."
