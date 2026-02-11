{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
-- For TH, for some reason

-- | Provides functionality for parsing command line arguments.
module Shrun.Configuration.Args.Parsing
  ( Args (..),
    parserInfoArgs,
    parserPrefs,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Version (showVersion)
import Effects.Optparse (validOsPath)
import Effects.Optparse.Completer qualified as EOC
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
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ParserPrefs)
import Paths_shrun qualified as Paths
import Shrun.Configuration.Args.Parsing.Core qualified as Core
import Shrun.Configuration.Args.Parsing.Graph qualified as Graph
import Shrun.Configuration.Args.Parsing.TH qualified as TH
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.Graph (EdgeArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import System.Info qualified as Info

-- | CLI args.
data Args = MkArgs
  { -- | Optional config file.
    configPaths :: Seq (WithDisabled OsPath),
    -- | Core config.
    coreConfig :: CoreConfigArgs,
    -- | List of commands.
    commands :: NESeq Text,
    -- | Whether to print the config.
    dryRun :: Bool,
    -- | Command dependencies.
    edges :: Maybe (WithDisabled EdgeArgs)
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Seq (WithDisabled OsPath), b ~ Seq (WithDisabled OsPath)) =>
  LabelOptic "configPaths" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3 a4 a5) ->
    fmap (\b -> MkArgs b a2 a3 a4 a5) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ CoreConfigArgs, b ~ CoreConfigArgs) =>
  LabelOptic "coreConfig" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3 a4 a5) ->
    fmap (\b -> MkArgs a1 b a3 a4 a5) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NESeq Text, b ~ NESeq Text) =>
  LabelOptic "commands" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3 a4 a5) ->
    fmap (\b -> MkArgs a1 a2 b a4 a5) (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "dryRun" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3 a4 a5) ->
    fmap (\b -> MkArgs a1 a2 a3 b a5) (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe (WithDisabled EdgeArgs), b ~ Maybe (WithDisabled EdgeArgs)) =>
  LabelOptic "edges" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3 a4 a5) ->
    fmap (\b -> MkArgs a1 a2 a3 a4 b) (f a5)
  {-# INLINE labelOptic #-}

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: List String -> ParserInfo Args
parserInfoArgs prevKeys =
  ParserInfo
    { infoParser = argsParser prevKeys,
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
              [ "CLI options override toml config so e.g. ",
                "'--console-log-command off' will disable ",
                "command logging, regardless of the toml's console-log.command."
              ],
          completions,
          Chunk.paragraph "See github.com/tbidne/shrun#README for full documentation.",
          Chunk.paragraph "Examples:",
          mkExample
            [ "1. Runs cmd1, cmd2, cmd3 concurrently:",
              "",
              "$ shrun cmd1 cmd2 cmd3",
              "[Command][cmd1] cmd1 output...",
              "[Command][cmd2] cmd2 output...",
              "[Command][cmd3] cmd3 output...",
              "[Status][0|3|0|0] 5 seconds"
            ],
          mkExample
            [ "2. Uses --edges to specify command dependencies. Commands cmd1 and",
              "cmd2 are run concurrently; cmd3 is started after cmd1 and cmd2 finish",
              "successfully.",
              "",
              "$ shrun --edges \"1 & 3, 2 & 3\" cmd1 cmd2 cmd3",
              "[Command][cmd1] cmd1 output...",
              "[Command][cmd2] cmd2 output...",
              "[Status][1|2|0|0] 5 seconds"
            ],
          mkExample
            [ "3. Uses config file aliases i.e. builds frontend, backend, and db",
              "concurrently, then runs deploy if those tasks completed successfully.",
              "",
              "# config.toml",
              "legend = [",
              "  # Aliases for multiple commands",
              "  { key = 'deploy', val = [ 'build', 'ds' ], edges = '1 & 2' },",
              "  { key = 'build', val = [ 'frontend', 'backend', 'db' ] },",
              "",
              "  # Aliases to actual commands",
              "  { key = 'frontend', val = 'npm run build' },",
              "  { key = 'backend', val = 'javac ...' },",
              "  { key = 'db', val = 'db.sh' },",
              "  { key = 'ds', val = 'deploy.sh' },",
              "]",
              "",
              "$ shrun --config config.toml deploy",
              "[Command][frontend] Running npm...",
              "[Command][backend] Running javac...",
              "[Command][db] Running db.sh...",
              "[Status][1|3|0|0] 5 seconds"
            ]
        ]

    completions =
      Chunk.vcatChunks
        [ Chunk.paragraph
            $ mconcat
              [ "Shrun also supports tab-completions for bash, fish, and zsh. ",
                "To load them, run the appropriate script:"
              ],
          Pretty.nest 2
            <$> Chunk.vcatChunks
              [ toChunk 0 "",
                Chunk.stringChunk "$ source <(shrun --bash-completion-script `which shrun`)",
                Chunk.stringChunk "$ source <(shrun --fish-completion-script `which shrun`)",
                Chunk.stringChunk "$ source <(shrun --zsh-completion-script `which shrun`)"
              ]
        ]

    mkExample :: NonEmpty String -> Chunk DocOA
    mkExample = identPara 2 5

    identPara :: Int -> Int -> NonEmpty String -> Chunk DocOA
    identPara hIndent lIndent (h :| xs) =
      Chunk.vcatChunks
        . (\ys -> toChunk hIndent h : ys)
        . fmap (toChunk lIndent)
        $ xs

    toChunk _ "" = line
    toChunk i other = fmap (Pretty.indent i) . Chunk.stringChunk $ other

    line = Chunk (Just Pretty.softline)

parserPrefs :: ParserPrefs
parserPrefs =
  OA.prefs
    $ mconcat
      [ OA.helpIndent 6
      ]

argsParser :: List String -> Parser Args
argsParser prevKeys = do
  configPaths <- configParser
  edges <- Graph.edgesParser
  coreConfig <-
    Core.coreParser
      <**> version
      <**> OA.helper

  dryRun <- miscParser

  commands <- commandsParser prevKeys

  pure
    $ MkArgs
      { configPaths,
        coreConfig,
        commands,
        edges,
        dryRun
      }
  where
    -- Want to put this in its own group. We put defaultConfig first for
    -- alphabetical order.
    miscParser =
      OA.parserOptionGroup
        "Miscellaneous options:"
        $ defaultConfig
        <*> dryRunParser

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
      ghc = showVersion Info.fullCompilerVersion,
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
    help = "Writes a default toml config file to stdout."

configParser :: Parser (Seq (WithDisabled OsPath))
configParser =
  fmap listToSeq
    . many
    $ Utils.withDisabledParserNoOpts
      validOsPath
      opts
  where
    opts =
      [ OA.long "config",
        OA.short 'c',
        OA.metavar "(PATH | off)...",
        OA.completer EOC.compgenCwdPathsCompleter,
        OA.completeWith ["off"],
        helpTxt
      ]

    helpTxt =
      OA.helpDoc
        . Chunk.unChunk
        $ Chunk.vcatChunks
          [ helpList,
            Utils.toChunk Pretty.softline,
            outtro,
            Utils.toChunk Pretty.softline
          ]

    helpList =
      Utils.itemizeHelper
        $ intro
        :<|| [ "<XDG_config>/shrun/config.toml",
               "./.shrun.toml",
               "./shrun.toml"
             ]

    intro =
      mconcat
        [ "Path(s) to TOML config file(s). This argument can be given multiple ",
          "times, in which case all keys are merged. When there is a conflict, ",
          "the right-most config wins. The legends are also merged, with the ",
          "same right-bias for conflicting keys. The string 'off' disables ",
          "all config files to its left. Finally, we also search in specific ",
          "locations automatically. These are:"
        ]

    outtro =
      Chunk.paragraph
        $ mconcat
          [ "These files are considered 'left' of any configs explicitly given ",
            "with --config, hence disabled with 'off'."
          ]

commandsParser :: List String -> Parser (NESeq Text)
commandsParser prevKeys =
  unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str opts
      )
  where
    opts =
      mconcat
        [ OA.metavar "Commands...",
          OA.completeWith prevKeys,
          OA.completer EOC.compgenCwdPathsCompleter
        ]

dryRunParser :: Parser Bool
dryRunParser =
  OA.switch
    $ mconcat
      [ OA.long "dry-run",
        Utils.mkHelpNoLine "Prints the configuration and commands that would be run to stdout, then exits."
      ]
