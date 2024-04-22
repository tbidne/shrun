-- | CLI parsing for CmdLoggingArgs
module Shrun.Configuration.Args.Parsing.CmdLogging
  ( cmdLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.CmdLogging
  ( CmdLoggingArgs,
    CmdLoggingP
      ( MkCmdLoggingP,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.StripControl qualified as StripControl
import Shrun.Data.Truncation (LineTruncation)
import Shrun.Data.Truncation qualified as Trunc
import Shrun.Prelude

cmdLoggingParser :: Parser CmdLoggingArgs
cmdLoggingParser = do
  stripControl <- cmdLogStripControlParser
  lineTrunc <- cmdLogLineTruncParser

  pure
    $ MkCmdLoggingP
      { stripControl,
        lineTrunc
      }

cmdLogStripControlParser :: Parser (WithDisabled StripControl)
cmdLogStripControlParser =
  Utils.withDisabledParser mainParser "cmd-log-strip-control"
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "cmd-log-strip-control",
                OA.short 's',
                Utils.mkHelp helpTxt,
                OA.metavar "(all | smart | none)"
              ]
          )
    helpTxt =
      mconcat
        [ "Control characters can wreak layout havoc with the --cmd-log",
          " option, thus we include this option. 'all' strips all",
          " such chars. 'none' does nothing i.e. all chars are left",
          " untouched. The default 'smart' attempts to strip",
          " only the control chars that affect layout (e.g. cursor movements) and",
          " leaves others unaffected (e.g. colors). This has the potential",
          " to be the 'prettiest' though it is possible to miss some chars.",
          " This option is experimental and subject to change."
        ]

cmdLogLineTruncParser :: Parser (WithDisabled LineTruncation)
cmdLogLineTruncParser = Utils.withDisabledParser mainParser "cmd-log-line-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseLineTruncation OA.auto OA.str)
          ( mconcat
              [ OA.long "cmd-log-line-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | detect)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of logs ",
          "produced via --cmd-log in the console logs. Can also be the ",
          "string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation. This does ",
          "not affect file logs with --file-log."
        ]
