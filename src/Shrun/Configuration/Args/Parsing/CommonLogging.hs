-- | CLI parsing for CommonLoggingArgs
module Shrun.Configuration.Args.Parsing.CommonLogging
  ( commonLoggingParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingArgs,
    CommonLoggingP (MkCommonLoggingP, keyHide, timerFormat),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Data.KeyHide (KeyHide (KeyHideOn))
import Shrun.Data.TimerFormat (TimerFormat)
import Shrun.Data.TimerFormat qualified as TimerFormat
import Shrun.Prelude

commonLoggingParser :: Parser CommonLoggingArgs
commonLoggingParser = do
  keyHide <- keyHideParser
  timerFormat <- timerFormatParser

  pure
    $ MkCommonLoggingP
      { keyHide,
        timerFormat
      }

keyHideParser :: Parser (WithDisabled KeyHide)
keyHideParser = Utils.withDisabledParser mainParser "log-key-hide"
  where
    mainParser =
      OA.optional
        $ OA.flag'
          KeyHideOn
          ( mconcat
              [ OA.long "log-key-hide",
                Utils.mkHelp helpTxt
              ]
          )
    helpTxt =
      mconcat
        [ "By default, we display the key name from the legend over the ",
          "actual command that was run, if the former exists. This flag ",
          "instead shows the literal command. Commands without keys are ",
          "unaffected."
        ]

timerFormatParser :: Parser (WithDisabled TimerFormat)
timerFormatParser = Utils.withDisabledParser mainParser "log-timer-format"
  where
    mainParser =
      OA.optional
        $ OA.option (TimerFormat.parseTimerFormat OA.str)
        $ mconcat
          [ OA.long "log-timer-format",
            Utils.mkHelp helpTxt,
            OA.metavar TimerFormat.timerFormatStr
          ]
    helpTxt =
      mconcat
        [ "How to format the timer. Defaults to prose_compact e.g. ",
          "'2 hours, 3 seconds'."
        ]
