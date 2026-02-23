-- | CLI parsing for FileLoggingArgs
module Shrun.Configuration.Args.Parsing.FileLogging
  ( fileLoggingParser,
  )
where

import Effects.Optparse.Completer qualified as EOC
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.FileLogging
  ( DeleteOnSuccessSwitch (MkDeleteOnSuccessSwitch),
    FileLogInitP
      ( MkFileLogInitP,
        mode,
        path,
        sizeMode
      ),
    FileLoggingArgs,
    FileLoggingP
      ( MkFileLoggingP,
        commandNameTrunc,
        deleteOnSuccess,
        file,
        lineTrunc,
        stripControl
      ),
  )
import Shrun.Configuration.Data.FileLogging.FileMode (FileMode)
import Shrun.Configuration.Data.FileLogging.FileMode qualified as FileMode
import Shrun.Configuration.Data.FileLogging.FilePathDefault (FilePathDefault)
import Shrun.Configuration.Data.FileLogging.FilePathDefault qualified as FilePathDefault
import Shrun.Configuration.Data.FileLogging.FileSizeMode (FileSizeMode)
import Shrun.Configuration.Data.FileLogging.FileSizeMode qualified as FileSizeMode
import Shrun.Configuration.Data.StripControl (FileLogStripControl)
import Shrun.Configuration.Data.StripControl qualified as StripControl
import Shrun.Configuration.Data.Truncation
  ( LineTruncation,
    TruncRegion (TruncCommandName),
    Truncation,
  )
import Shrun.Configuration.Data.Truncation qualified as Trunc
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

fileLoggingParser :: Parser FileLoggingArgs
fileLoggingParser = do
  path <- fileLogParser
  commandNameTrunc <- fileLogCommandNameTruncParser
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
        commandNameTrunc,
        deleteOnSuccess,
        lineTrunc,
        stripControl
      }

fileLogParser :: Parser (Maybe (WithDisabled FilePathDefault))
fileLogParser =
  Utils.mWithDisabledParser
    (OA.str >>= FilePathDefault.parseFilePathDefault)
    opts
    "default | PATH"
  where
    opts =
      [ OA.long "file-log",
        OA.completeWith ["default"],
        OA.completer EOC.compgenCwdDirsCompleter,
        OA.short 'f',
        Utils.mkHelp helpTxt
      ]
    helpTxt =
      mconcat
        [ "If a path is supplied, all logs will additionally be written to ",
          "the supplied file. Furthermore, 'command' logs will be written to ",
          "the file irrespective of --console-log-command. Console logging is ",
          "unaffected. This can be useful for investigating command failures. ",
          "If the string 'default' is given, then we write to the XDG state ",
          "directory e.g. ~/.local/state/shrun/shrun.log."
        ]

fileLogCommandNameTruncParser :: Parser (Maybe (WithDisabled (Truncation TruncCommandName)))
fileLogCommandNameTruncParser =
  Utils.mWithDisabledParser
    (Trunc.parseTruncation Utils.autoStripUnderscores)
    opts
    "NATURAL"
  where
    opts =
      [ OA.long "file-log-command-name-trunc",
        Utils.mkHelp helpTxt
      ]
    helpTxt = "Like --console-log-command-name-trunc, but for --file-logs."

deleteOnSuccessParser :: Parser (Maybe DeleteOnSuccessSwitch)
deleteOnSuccessParser =
  Utils.switchParser MkDeleteOnSuccessSwitch "file-log-delete-on-success" helpTxt
  where
    helpTxt =
      mconcat
        [ "If --file-log is active, deletes the file on a successful exit. ",
          "Does not delete the file if shrun exited via failure."
        ]

lineTruncParser :: Parser (Maybe (WithDisabled LineTruncation))
lineTruncParser =
  Utils.mWithDisabledParser
    (Trunc.parseLineTruncation Utils.autoStripUnderscores OA.str)
    opts
    Trunc.lineTruncStr
  where
    opts =
      [ OA.long "file-log-line-trunc",
        OA.completeWith ["detect"],
        Utils.mkHelp helpTxt
      ]
    helpTxt = "Like --console-log-line-trunc, but for --file-log. Defaults to 'off'."

fileLogStripControlParser :: Parser (Maybe FileLogStripControl)
fileLogStripControlParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (StripControl.parseStripControl OA.str)
          ( mconcat
              [ OA.long "file-log-strip-control",
                OA.completeWith ["all", "smart", "off"],
                Utils.mkHelpNoLine helpTxt,
                OA.metavar StripControl.stripControlStr
              ]
          )
    helpTxt =
      mconcat
        [ "--console-log-strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

fileLogModeParser :: Parser (Maybe FileMode)
fileLogModeParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileMode.parseFileMode OA.str)
          ( mconcat
              [ OA.long "file-log-mode",
                OA.completeWith ["append", "rename", "write"],
                Utils.mkHelp helpTxt,
                OA.metavar FileMode.fileModeStr
              ]
          )
    helpTxt =
      mconcat
        [ "Mode in which to open the log file. Defaults to 'write'. The 'rename' ",
          "option will rename the requested log file if there is a collision ",
          "e.g. '-f shrun.log' will become 'shrun (1).log'."
        ]

fileLogSizeModeParser :: Parser (Maybe FileSizeMode)
fileLogSizeModeParser = mainParser
  where
    mainParser =
      OA.optional
        $ OA.option
          (FileSizeMode.parseFileSizeMode OA.str)
          ( mconcat
              [ OA.long "file-log-size-mode",
                OA.completeWith ["warn", "delete"],
                Utils.mkHelp helpTxt,
                OA.metavar FileSizeMode.fileSizeModeStr
              ]
          )
    helpTxt =
      mconcat
        [ "Sets a threshold for the file log size, upon which we either ",
          "print a warning or delete the file, if it is exceeded. ",
          "The BYTES should include the value and units e.g. ",
          "'warn 10 mb', 'warn 5 gigabytes', 'delete 20.5B'. Defaults to warning ",
          "at 50 mb."
        ]
