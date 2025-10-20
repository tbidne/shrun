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

  commonLogging <-
    OA.parserOptionGroup "Common Logging options:" CommonLogging.commonLoggingParser
  commandLogging <-
    OA.parserOptionGroup "Command Logging options:" CommandLogging.commandLoggingParser
  consoleLogging <-
    OA.parserOptionGroup "Console Logging options:" ConsoleLogging.consoleLoggingParser
  fileLogging <-
    OA.parserOptionGroup "File Logging options:" FileLogging.fileLoggingParser
  notify <- OA.parserOptionGroup "Notifications options:" Notify.notifyParser

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

timeoutParser :: Parser (Maybe (WithDisabled Timeout))
timeoutParser =
  Utils.mWithDisabledParser
    (Timeout.parseTimeout Utils.autoStripUnderscores OA.str)
    opts
    Timeout.timeoutStr
  where
    opts =
      [ OA.long "timeout",
        OA.short 't',
        Utils.mkHelp helpTxt
      ]
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

initParser :: Parser (Maybe (WithDisabled Text))
initParser =
  Utils.mWithDisabledParser
    OA.str
    opts
    "STRING"
  where
    opts =
      [ OA.long "init",
        OA.short 'i',
        Utils.mkHelp helpTxt
      ]
    helpTxt =
      mconcat
        [ "If given, init is run before each command. That is, ",
          "'shrun --init \". ~/.bashrc\" foo bar' is equivalent ",
          "to 'shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"'."
        ]
