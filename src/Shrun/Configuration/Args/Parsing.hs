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
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import Shrun.Utils qualified as U

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
  labelOptic = lensVL $ \f (MkArgs _configPath _coreConfig _commands) ->
    fmap (\configPath' -> MkArgs configPath' _coreConfig _commands) (f _configPath)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ CoreConfigArgs, b ~ CoreConfigArgs) =>
  LabelOptic "coreConfig" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs _configPath _coreConfig _commands) ->
    fmap (\coreConfig' -> MkArgs _configPath coreConfig' _commands) (f _coreConfig)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NESeq Text, b ~ NESeq Text) =>
  LabelOptic "commands" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs _configPath _coreConfig _commands) ->
    fmap (MkArgs _configPath _coreConfig) (f _commands)
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
                "example, the --no-console-log-command option will instruct shrun to ",
                "ignore both cli --console-log-command and toml console-log.command, ",
                "ensuring the default behavior is used (i.e. no command logging)."
              ],
          Chunk.paragraph "See github.com/tbidne/shrun#README for full documentation."
        ]

argsParser :: Parser Args
argsParser = do
  MkArgs
    <$> configParser
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
  U.unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )
