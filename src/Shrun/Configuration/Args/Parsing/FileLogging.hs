-- | CLI parsing for FileLoggingArgs
module Shrun.Configuration.Args.Parsing.FileLogging
  ( fileLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.FileLogging
  ( FileLogInitP
      ( MkFileLogInitP,
        mode,
        path,
        sizeMode
      ),
    FileLoggingArgs,
    FileLoggingP
      ( MkFileLoggingP,
        cmdNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Data.FileMode (FileMode)
import Shrun.Data.FileMode qualified as FileMode
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FilePathDefault qualified as FilePathDefault
import Shrun.Data.FileSizeMode (FileSizeMode)
import Shrun.Data.FileSizeMode qualified as FileSizeMode
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.StripControl qualified as StripControl
import Shrun.Data.Truncation (LineTruncation, TruncRegion (TCmdName), Truncation)
import Shrun.Data.Truncation qualified as Trunc
import Shrun.Prelude

fileLoggingParser :: Parser FileLoggingArgs
fileLoggingParser = do
  path <- fileLogParser
  cmdNameTrunc <- fileLogCmdNameTruncParser
  deleteOnSuccess <- deleteOnSuccessParser
  lineTrunc <- lineTruncParser
  mode <- fileLogModeParser
  sizeMode <- fileLogSizeModeParser
  stripControl <- fileLogStripControlParser

  pure
    $ MkFileLoggingP
      { file =
          MkFileLogInitP
            { mode,
              path,
              sizeMode
            },
        cmdNameTrunc,
        deleteOnSuccess,
        lineTrunc,
        stripControl
      }

fileLogParser :: Parser (WithDisabled FilePathDefault)
fileLogParser = Utils.withDisabledParser mainParser "file-log"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FilePathDefault.parseFilePathDefault OA.str)
          ( mconcat
              [ OA.long "file-log",
                OA.short 'f',
                Utils.mkHelp helpTxt,
                OA.metavar "(default | PATH)"
              ]
          )
    helpTxt =
      mconcat
        [ "If a path is supplied, all logs will additionally be written to ",
          "the supplied file. Furthermore, command logs will be written to ",
          "the file irrespective of --cmd-log. Console logging is unaffected. ",
          "This can be useful for investigating command failures. ",
          "If the string 'default' is given, then we write to the XDG state ",
          "directory e.g. ~/.local/state/shrun/shrun.log."
        ]

fileLogCmdNameTruncParser :: Parser (WithDisabled (Truncation TCmdName))
fileLogCmdNameTruncParser =
  Utils.withDisabledParser mainParser "file-log-cmd-name-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseTruncation OA.auto)
          ( mconcat
              [ OA.long "file-log-cmd-name-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "NATURAL"
              ]
          )
    helpTxt = "Like --console-log-cmd-name-trunc, but for --file-logs."

deleteOnSuccessParser :: Parser (WithDisabled ())
deleteOnSuccessParser = Utils.withDisabledParser mainParser "file-log-delete-on-success"
  where
    switchParser =
      OA.switch
        ( mconcat
            [ OA.long "file-log-delete-on-success",
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
        [ "If --file-log is active, deletes the file on a successful exit. ",
          "Does not delete the file if shrun exited via failure."
        ]

lineTruncParser :: Parser (WithDisabled LineTruncation)
lineTruncParser = Utils.withDisabledParser mainParser "file-log-line-trunc"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Trunc.parseLineTruncation OA.auto OA.str)
          ( mconcat
              [ OA.long "file-log-line-trunc",
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | detect)"
              ]
          )
    helpTxt = "Like --console-log-line-trunc, but for --file-log."

fileLogStripControlParser :: Parser (WithDisabled StripControl)
fileLogStripControlParser =
  Utils.withDisabledParser mainParser "file-log-strip-control"
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "file-log-strip-control",
                Utils.mkHelp helpTxt,
                OA.metavar "(all | smart | none)"
              ]
          )
    helpTxt =
      mconcat
        [ "--console-log-strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

fileLogModeParser :: Parser (WithDisabled FileMode)
fileLogModeParser = Utils.withDisabledParser mainParser "file-log-mode"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileMode.parseFileMode OA.str)
          ( mconcat
              [ OA.long "file-log-mode",
                Utils.mkHelp helpTxt,
                OA.metavar "(append | write)"
              ]
          )
    helpTxt = "Mode in which to open the log file. Defaults to write."

fileLogSizeModeParser :: Parser (WithDisabled FileSizeMode)
fileLogSizeModeParser = Utils.withDisabledParser mainParser "file-log-size-mode"
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileSizeMode.parseFileSizeMode OA.str)
          ( mconcat
              [ OA.long "file-log-size-mode",
                Utils.mkHelp helpTxt,
                OA.metavar FileSizeMode.expectedStr
              ]
          )
    helpTxt =
      mconcat
        [ "Sets a threshold for the file log size, upon which we either ",
          "print a warning or delete the file, if it is exceeded. ",
          "The SIZE should include the value and units e.g. ",
          "warn 10 mb, warn 5 gigabytes, delete 20.5B. Defaults to warning ",
          "at 50 mb."
        ]
