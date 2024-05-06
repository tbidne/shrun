-- | CLI parsing for ConsoleLoggingArgs
module Shrun.Configuration.Args.Parsing.ConsoleLogging
  ( consoleLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.ConsoleLogging
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.StripControl qualified as StripControl
import Shrun.Configuration.Data.Truncation (LineTruncation, TruncRegion (TruncCommandName), Truncation)
import Shrun.Configuration.Data.Truncation qualified as Trunc
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

consoleLoggingParser :: Parser ConsoleLoggingArgs
consoleLoggingParser = do
  commandLogging <- commandLoggingParser
  commandNameTrunc <- commandNameTruncParser
  lineTrunc <- lineTruncParser
  stripControl <- stripControlParser

  pure
    $ MkConsoleLoggingP
      { commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl
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
          (Trunc.parseTruncation OA.auto)
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
          (Trunc.parseLineTruncation OA.auto OA.str)
          ( mconcat
              [ OA.long "console-log-line-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | detect)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of console logs. Can ",
          "also be the string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation."
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
                OA.metavar "(all | smart | none)"
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
