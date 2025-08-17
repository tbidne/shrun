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
    CommonLoggingP (MkCommonLoggingP),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOn),
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude

commonLoggingParser :: Parser CommonLoggingArgs
commonLoggingParser = MkCommonLoggingP <$> keyHideParser

keyHideParser :: Parser (WithDisabled KeyHideSwitch)
keyHideParser = Utils.withDisabledParserNoLine mainParser "common-log-key-hide"
  where
    mainParser =
      OA.optional
        $ OA.flag'
          KeyHideOn
          ( mconcat
              [ OA.long "common-log-key-hide",
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
