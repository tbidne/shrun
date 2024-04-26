-- | CLI parsing for ConsoleLoggingArgs
module Shrun.Configuration.Args.Parsing.ConsoleLogging
  ( consoleLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.ConsoleLogging
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.StripControl qualified as StripControl
import Shrun.Data.Truncation (LineTruncation, TruncRegion (TCmdName), Truncation)
import Shrun.Data.Truncation qualified as Trunc
import Shrun.Prelude

consoleLoggingParser :: Parser ConsoleLoggingArgs
consoleLoggingParser = do
  cmdLogging <- cmdLoggingParser
  cmdNameTrunc <- cmdNameTruncParser
  lineTrunc <- lineTruncParser
  stripControl <- stripControlParser

  pure
    $ MkConsoleLoggingP
      { cmdLogging,
        cmdNameTrunc,
        lineTrunc,
        stripControl
      }

cmdLoggingParser :: Parser (WithDisabled ())
cmdLoggingParser = Utils.withDisabledParser mainParser "console-log-cmd"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "console-log-cmd",
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

cmdNameTruncParser :: Parser (WithDisabled (Truncation TCmdName))
cmdNameTruncParser = Utils.withDisabledParser mainParser "console-log-cmd-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation OA.auto)
          ( mconcat
              [ OA.long "console-log-cmd-name-trunc",
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

stripControlParser :: Parser (WithDisabled StripControl)
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
