{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
-- For TH, for some reason

-- | Provides functionality for parsing command line arguments.
module Shrun.Configuration.Args.Parsing
  ( Args (..),
    parserInfoArgs,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Version (Version (versionBranch))
import Effects.Optparse (validOsPath)
import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_shrun qualified as Paths
import Shrun.Configuration.Args.Parsing.Core qualified as Core
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Args.TH (getDefaultConfigTH)
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.WithDisable (WithDisable)
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | CLI args.
data Args = MkArgs
  { -- | Optional config file.
    configPath :: WithDisable (Maybe OsPath),
    -- | Whether to log commands.
    cmdLog :: WithDisable Bool,
    -- | Core config.
    coreConfig :: CoreConfigArgs,
    -- | List of commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Args

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Shrun: A tool for running shell commands concurrently."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph
            $ mconcat
              [ "Shrun runs shell commands concurrently. In addition to providing ",
                "basic timing and logging functionality, we also provide the ",
                "ability to pass in a config file that can be used to define ",
                "aliases for commands."
              ],
          Chunk.paragraph
            $ mconcat
              [ "In general, each option --foo has a --no-foo variant that ",
                "disables cli and toml configuration for that field. For ",
                "example, the --no-cmd-log option will instruct shrun to ",
                "ignore both cli --cmd-log and toml cmd-log, ensuring the ",
                "default behavior is used (i.e. no command logging)."
              ],
          Chunk.paragraph "See github.com/tbidne/shrun#README for full documentation."
        ]

argsParser :: Parser Args
argsParser = do
  MkArgs
    <$> configParser
    <*> cmdLogParser
    <*> Core.coreParser
    <**> defaultConfig
    <**> version
    <**> OA.helper
    <*> commandsParser

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

defaultConfig :: Parser (a -> a)
defaultConfig = OA.infoOption (unpack txt) (OA.long "default-config" <> Utils.mkHelp help)
  where
    txt = T.unlines $$getDefaultConfigTH
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (WithDisable (Maybe OsPath))
configParser = Utils.withDisableParser mainParser "config"
  where
    mainParser =
      OA.optional
        $ OA.option
          validOsPath
          ( mconcat
              [ OA.long "config",
                OA.short 'c',
                Utils.mkHelp mainHelpTxt,
                OA.metavar "PATH"
              ]
          )
    mainHelpTxt =
      mconcat
        [ "Path to TOML config file. If this argument is not given ",
          "we automatically look in the XDG config directory ",
          "e.g. ~/.config/shrun/config.toml. The --no-config option disables ",
          "--config and the automatic XDG lookup."
        ]

cmdLogParser :: Parser (WithDisable Bool)
cmdLogParser = Utils.withDisableParser mainParser "cmd-log"
  where
    mainParser =
      OA.switch
        ( mconcat
            [ OA.short 'l',
              OA.long "cmd-log",
              Utils.mkHelp helpTxt
            ]
        )
    helpTxt =
      mconcat
        [ "The default behavior is to swallow logs for the commands ",
          "themselves. This flag gives each command a console region in ",
          "which its logs will be printed. Only the latest log per region ",
          "is show at a given time."
        ]

commandsParser :: Parser (NESeq Text)
commandsParser =
  U.unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )
