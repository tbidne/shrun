{-# LANGUAGE OverloadedLists #-}

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
        legendKeysCache,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.Core.Timeout qualified as Timeout
import Shrun.Configuration.Data.LegendKeysCache (LegendKeysCache)
import Shrun.Configuration.Data.LegendKeysCache qualified as LKS
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

coreParser :: Parser CoreConfigArgs
coreParser = do
  init <- initParser
  legendKeysCache <- legendKeysCacheParser
  timeout <- timeoutParser

  ~( commonLogging,
     commandLogging,
     consoleLogging,
     fileLogging
     ) <-
    OA.parserOptionGroup "Logging options:"
      $ (,,,)
      <$> OA.parserOptionGroup "Common (console and file logs):" CommonLogging.commonLoggingParser
      <*> OA.parserOptionGroup "Command ('command' logs):" CommandLogging.commandLoggingParser
      <*> OA.parserOptionGroup "Console:" ConsoleLogging.consoleLoggingParser
      <*> OA.parserOptionGroup "File:" FileLogging.fileLoggingParser

  notify <- OA.parserOptionGroup "Notifications options:" Notify.notifyParser

  pure
    $ MkCoreConfigP
      { timeout,
        init,
        legendKeysCache,
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

legendKeysCacheParser :: Parser (Maybe LegendKeysCache)
legendKeysCacheParser =
  OA.optional
    $ OA.option
      (LKS.parseLegendKeysCache OA.str)
      opts
  where
    opts =
      mconcat
        [ OA.long "legend-keys-cache",
          OA.completeWith ["add", "clear", "write", "off"],
          OA.metavar LKS.lksStrings,
          helpTxt
        ]

    helpTxt =
      Utils.itemize
        $ intro
        :<|| [ add,
               clear,
               write
             ]

    intro =
      mconcat
        [ "Shrun allows saving legend keys from the current config file so ",
          "that we can get tab-completions on the next run."
        ]

    add = "add: The default. Combines keys from this run with the prior run(s)."
    write = "write: Saves keys from this run only."
    clear = "clear: Deletes the keys cache, if it exists."
