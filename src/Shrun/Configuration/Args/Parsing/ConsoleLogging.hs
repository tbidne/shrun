-- | CLI parsing for ConsoleLoggingArgs
module Shrun.Configuration.Args.Parsing.ConsoleLogging
  ( consoleLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLoggingArgs,
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

commandLoggingParser :: Parser (WithDisabled ())
commandLoggingParser = Utils.withDisabledParser mainParser "console-log-command"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "console-log-command",
              Utils.mkHelp helpTxt
            ]
        )
    mainParser = do
      b <- switchParser
      pure
        $ if b
          then Just ()
          else Nothing
    helpTxt =
      mconcat
        [ "The default behavior is to swallow logs for the commands ",
          "themselves. This flag gives each command a console region in ",
          "which its logs will be printed. Only the latest log per region ",
          "is show at a given time."
        ]

commandNameTruncParser :: Parser (WithDisabled (Truncation TruncCommandName))
commandNameTruncParser = Utils.withDisabledParser mainParser "console-log-command-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation Utils.autoStripUnderscores)
          ( mconcat
              [ OA.long "console-log-command-name-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation."
        ]

lineTruncParser :: Parser (WithDisabled LineTruncation)
lineTruncParser = Utils.withDisabledParser mainParser "console-log-line-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseLineTruncation Utils.autoStripUnderscores OA.str)
          ( mconcat
              [ OA.long "console-log-line-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar Trunc.lineTruncStr
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of console logs. Can ",
          "also be the string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation. Note that \"log prefixes\" ",
          "(e.g. labels like [Success], timestamps) are counted towards the ",
          "total length but are never truncated."
        ]

stripControlParser :: Parser (WithDisabled ConsoleLogStripControl)
stripControlParser =
  Utils.withDisabledParser mainParser "console-log-strip-control"
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "console-log-strip-control",
                Utils.mkHelp helpTxt,
                OA.metavar StripControl.stripControlStr
              ]
          )
    helpTxt =
      mconcat
        [ "Control characters can wreak layout havoc, thus we include this",
          " option. 'all' strips all",
          " such chars. 'none' does nothing i.e. all chars are left",
          " untouched. The default 'smart' attempts to strip",
          " only the control chars that affect layout (e.g. cursor movements) and",
          " leaves others unaffected (e.g. colors). This has the potential",
          " to be the 'prettiest' though it is possible to miss some chars.",
          " This option is experimental and subject to change."
        ]

timerFormatParser :: Parser (WithDisabled TimerFormat)
timerFormatParser = Utils.withDisabledParserNoLine mainParser "console-log-timer-format"
  where
    mainParser =
      OA.optional
        $ OA.option (TimerFormat.parseTimerFormat OA.str)
        $ mconcat
          [ OA.long "console-log-timer-format",
            Utils.mkHelp helpTxt,
            OA.metavar TimerFormat.timerFormatStr
          ]
    helpTxt =
      mconcat
        [ "How to format the timer. Defaults to prose_compact e.g. ",
          "'2 hours, 3 seconds'."
        ]
