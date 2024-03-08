-- | CLI parsing for FileLoggingArgs
module Shrun.Configuration.Args.Parsing.FileLogging
  ( fileLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.FileLogging
  ( FileLoggingArgs,
    FileLoggingP (MkFileLoggingP, mode, path, sizeMode, stripControl),
  )
import Shrun.Configuration.Data.WithDisable (WithDisable)
import Shrun.Data.FileMode (FileMode)
import Shrun.Data.FileMode qualified as FileMode
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.FilePathDefault qualified as FilePathDefault
import Shrun.Data.FileSizeMode (FileSizeMode)
import Shrun.Data.FileSizeMode qualified as FileSizeMode
import Shrun.Data.StripControl (StripControl)
import Shrun.Data.StripControl qualified as StripControl
import Shrun.Prelude

fileLoggingParser :: Parser FileLoggingArgs
fileLoggingParser = do
  path <- fileLogParser
  stripControl <- fileLogStripControlParser
  mode <- fileLogModeParser
  sizeMode <- fileLogSizeModeParser

  pure
    $ MkFileLoggingP
      { path,
        stripControl,
        mode,
        sizeMode
      }

fileLogParser :: Parser (WithDisable (Maybe FilePathDefault))
fileLogParser = Utils.withDisableParser mainParser "file-log"
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
          "If the string 'default' is given, then we write to the XDG config ",
          "directory e.g. ~/.config/shrun/shrun.log."
        ]

fileLogStripControlParser :: Parser (WithDisable (Maybe StripControl))
fileLogStripControlParser =
  Utils.withDisableParser mainParser "file-log-strip-control"
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
        [ "--cmd-log-strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

fileLogModeParser :: Parser (WithDisable (Maybe FileMode))
fileLogModeParser = Utils.withDisableParser mainParser "file-log-mode"
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

fileLogSizeModeParser :: Parser (WithDisable (Maybe FileSizeMode))
fileLogSizeModeParser = Utils.withDisableParser mainParser "file-log-size-mode"
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
