-- | CLI parsing for CommonLoggingArgs
module Shrun.Configuration.Args.Parsing.CommonLogging
  ( commonLoggingParser,
  )
where

import Options.Applicative (Parser)
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingArgs,
    CommonLoggingP (MkCommonLoggingP),
    Debug (MkDebug),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (MkKeyHideSwitch),
  )
import Shrun.Prelude

commonLoggingParser :: Parser CommonLoggingArgs
commonLoggingParser = MkCommonLoggingP <$> debugParser <*> keyHideParser

debugParser :: Parser (Maybe Debug)
debugParser = Utils.switchParser MkDebug "common-log-debug" helpTxt
  where
    helpTxt = "Enables additional debug logging."

keyHideParser :: Parser (Maybe KeyHideSwitch)
keyHideParser = Utils.switchParserNoLine MkKeyHideSwitch "common-log-key-hide" helpTxt
  where
    helpTxt =
      mconcat
        [ "By default, we display the key name from the legend over the ",
          "actual command that was run, if the former exists. This flag ",
          "instead shows the literal command. Commands without keys are ",
          "unaffected."
        ]
