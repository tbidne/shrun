-- | CLI parsing for CoreConfigArgs
module Shrun.Configuration.Args.Parsing.Core
  ( coreParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.CommandLogging qualified as CommandLogging
import Shrun.Configuration.Args.Parsing.CommonLogging qualified as CommonLogging
import Shrun.Configuration.Args.Parsing.ConsoleLogging qualified as ConsoleLogging
import Shrun.Configuration.Args.Parsing.FileLogging qualified as FileLogging
import Shrun.Configuration.Args.Parsing.Notify qualified as Notify
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Core
  ( CoreConfigArgs,
    CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

coreParser :: Parser CoreConfigArgs
coreParser = do
  init <- initParser
  timeout <- timeoutParser
  commonLogging <- CommonLogging.commonLoggingParser
  commandLogging <- CommandLogging.commandLoggingParser
  consoleLogging <- ConsoleLogging.consoleLoggingParser
  fileLogging <- FileLogging.fileLoggingParser
  notify <- Notify.notifyParser

  pure
    $ MkCoreConfigP
      { timeout,
        init,
        commonLogging,
        consoleLogging,
        commandLogging,
        fileLogging,
        notify
      }

timeoutParser :: Parser (WithDisabled Timeout)
timeoutParser = Utils.withDisabledParser mainParser "timeout"
  where
    mainParser =
      OA.optional
        $ OA.option
          (Timeout.parseTimeout Utils.autoStripUnderscores OA.str)
          ( mconcat
              [ OA.long "timeout",
                OA.short 't',
                Utils.mkHelp helpTxt,
                OA.metavar "(NATURAL | STRING)"
              ]
          )
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

initParser :: Parser (WithDisabled Text)
initParser = Utils.withDisabledParser mainParser "init"
  where
    mainParser =
      OA.optional
        $ OA.option OA.str
        $ mconcat
          [ OA.long "init",
            OA.short 'i',
            Utils.mkHelp helpTxt,
            OA.metavar "STRING"
          ]
    helpTxt =
      mconcat
        [ "If given, init is run before each command. That is, ",
          "'shrun --init \". ~/.bashrc\" foo bar' is equivalent ",
          "to 'shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"'."
        ]
