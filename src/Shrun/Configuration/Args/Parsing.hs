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
import Data.Text qualified as T
import Data.Version (showVersion)
import Effects.Optparse (validOsPath)
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as OsString
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
import Shrun.Configuration.Args.Parsing.TH qualified as TH
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import System.Info qualified as Info

-- | CLI args.
data Args = MkArgs
  { -- | Optional config file.
    configPath :: WithDisabled OsPath,
    -- | Core config.
    coreConfig :: CoreConfigArgs,
    -- | List of commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ WithDisabled OsPath, b ~ WithDisabled OsPath) =>
  LabelOptic "configPath" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs b a2 a3) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ CoreConfigArgs, b ~ CoreConfigArgs) =>
  LabelOptic "coreConfig" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs a1 b a3) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NESeq Text, b ~ NESeq Text) =>
  LabelOptic "commands" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs a1 a2 b) (f a3)
  {-# INLINE labelOptic #-}

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
    footerTxt = Just $ fromString versShort
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
                "example, the --no-console-log-command option will instruct shrun to ",
                "ignore both cli --console-log-command and toml console-log.command, ",
                "ensuring the default behavior is used (i.e. no command logging)."
              ],
          Chunk.paragraph "See github.com/tbidne/shrun#README for full documentation."
        ]

argsParser :: Parser Args
argsParser = do
  configPath <- configParser
  coreConfig <-
    Core.coreParser
      <**> defaultConfig
      <**> version
      <**> OA.helper
  commands <- commandsParser

  pure
    $ MkArgs
      { configPath,
        coreConfig,
        commands
      }

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Shrun: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Shrun: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> OsString.decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData

defaultConfig :: Parser (a -> a)
defaultConfig =
  OA.infoOption
    (unpack $$TH.defaultToml)
    (OA.long "default-config" <> Utils.mkHelp help)
  where
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (WithDisabled OsPath)
configParser = Utils.withDisabledParser mainParser "config"
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

commandsParser :: Parser (NESeq Text)
commandsParser =
  unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )
