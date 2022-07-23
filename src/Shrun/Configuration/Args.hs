{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command line arguments.
--
-- @since 0.5
module Shrun.Configuration.Args
  ( Args (..),
    FileMode (..),
    defaultArgs,
    parserInfoArgs,
  )
where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Time.Relative qualified as RelativeTime
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( ParseError (ErrorMsg),
    Parser,
    ParserInfo (..),
    ReadM,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Shrun.Configuration.Args.TH (getDefaultConfigTH)
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    LineTruncation (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Data.FilePathDefault (FilePathDefault (..))
import Shrun.Data.NonEmptySeq (NonEmptySeq (..))
import Shrun.Data.NonEmptySeq qualified as NESeq
import Shrun.Data.Timeout (Timeout (..))
import Shrun.Prelude

-- | File mode.
--
-- @since 0.5
data FileMode
  = FileModeAppend
  | FileModeWrite
  deriving stock
    ( -- | @since 0.5
      Eq,
      -- | @since 0.5
      Show
    )

-- | @since 0.5
instance Semigroup FileMode where
  FileModeAppend <> _ = FileModeAppend
  _ <> FileModeAppend = FileModeAppend
  l <> _ = l

-- | @since 0.5
instance Monoid FileMode where
  mempty = FileModeWrite

-- | @since 0.5
instance DecodeTOML FileMode where
  tomlDecoder =
    tomlDecoder @Text >>= \case
      "append" -> pure FileModeAppend
      "write" -> pure FileModeWrite
      bad -> fail $ "Unrecognized file-mode: " <> unpack bad

-- | Type for parsing command line args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Optional config file.
    --
    -- @since 0.5
    configPath :: !(Maybe FilePath),
    -- | Ignores toml config file.
    --
    -- @since 0.5
    noConfig :: Bool,
    -- | Timeout.
    --
    -- @since 0.1
    timeout :: !(Maybe Timeout),
    -- | Global option for logging. If it is true then all logging is
    -- disabled.
    --
    -- @since 0.1
    disableLogging :: !(Maybe Bool),
    -- | Whether to log commands.
    --
    -- @since 0.1
    cmdLogging :: !(Maybe CmdLogging),
    -- | Whether to display command by (key) name or command.
    --
    -- @since 0.1
    cmdDisplay :: !(Maybe CmdDisplay),
    -- | Determines to what extent we should remove control characters
    -- from logs.
    --
    -- @since 0.3
    stripControl :: !(Maybe StripControl),
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1
    cmdLineTrunc :: !(Maybe LineTruncation),
    -- | Optional path to log file.
    --
    -- @since 0.1
    fileLogging :: !(Maybe FilePathDefault),
    -- | Mode to use with the file log.
    --
    -- since 0.5
    fileLogMode :: Maybe FileMode,
    -- | 'stripControl' for the log file.
    --
    -- @since 0.5
    fileLogStripControl :: !(Maybe StripControl),
    -- | List of commands.
    --
    -- @since 0.1
    commands :: !(NonEmptySeq Text)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Args

-- | Default configuration.
--
-- @since 0.1
defaultArgs :: NonEmptySeq Text -> Args
defaultArgs cmds =
  MkArgs
    { cmdLogging = empty,
      fileLogging = empty,
      fileLogStripControl = empty,
      fileLogMode = empty,
      cmdDisplay = empty,
      configPath = empty,
      noConfig = False,
      stripControl = empty,
      timeout = empty,
      cmdNameTrunc = empty,
      cmdLineTrunc = empty,
      disableLogging = empty,
      commands = cmds
    }

-- | 'ParserInfo' type for parsing 'Args'.
--
-- @since 0.1
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Shrun: A tool for running shell commands concurrently."
    footerTxt = Just $ fromString versNum
    desc =
      Just $
        "\nShrun runs shell commands concurrently. In addition to "
          <> "providing basic timing and logging functionality, we also provide "
          <> "the ability to pass in a config file that can be used to define "
          <> "aliases for commands. See github.com/tbidne/shrun#README for "
          <> "full documentation."

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> configParser
    <*> noConfigParser
    <*> timeoutParser
    <*> disableLoggingParser
    <*> commandLoggingParser
    <*> commandDisplayParser
    <*> stripControlParser
    <*> cmdTruncationParser
    <*> lineTruncationParser
    <*> fileLoggingParser
    <*> fileLogModeParser
    <*> fileLogStripControlParser
    <*> commandsParser
    <**> OA.helper
    <**> version
    <**> defaultConfig
{-# INLINEABLE argsParser #-}

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version" <> OA.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Shrun",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: String
versNum = "Version: " <> $$(PV.packageVersionStringTH "shrun.cabal")

defaultConfig :: Parser (a -> a)
defaultConfig = OA.infoOption (unpack txt) (OA.long "default-config" <> OA.help help)
  where
    txt = T.unlines $$(getDefaultConfigTH)
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (Maybe FilePath)
configParser =
  OA.optional $
    OA.option
      OA.str
      ( mconcat
          [ OA.long "config",
            OA.short 'c',
            OA.help helpTxt,
            OA.metavar "PATH"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Path to TOML config file. If this argument is not given ",
          "we automatically look in the Xdg config directory ",
          "e.g. ~/.config/shrun/config.toml"
        ]

noConfigParser :: Parser Bool
noConfigParser =
  OA.flag
    False
    True
    ( mconcat
        [ OA.long "no-config",
          OA.help helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "Overrides toml file config regardless of how it was obtained i.e. ",
          "explicit --config or implicit reading of the Xdg config file. ",
          "Used for when a config file exists at the expected Xdg ",
          "location, but we want to ignore it."
        ]

timeoutParser :: Parser (Maybe Timeout)
timeoutParser =
  OA.optional $
    OA.option
      readTimeout
      ( mconcat
          [ OA.long "timeout",
            OA.short 't',
            OA.help helpTxt,
            OA.metavar "NATURAL"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Non-negative integer setting a timeout. Can either be a raw number ",
          "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or ",
          "2h3s. Defaults to no timeout."
        ]

readTimeout :: ReadM Timeout
readTimeout = do
  s <- OA.str
  case RelativeTime.fromString s of
    Left _ ->
      OA.readerAbort $
        ErrorMsg $
          "Could not parse timeout: " <> s
    Right t -> pure $ MkTimeout $ RelativeTime.toSeconds t

cmdTruncationParser :: Parser (Maybe (Truncation 'TCmdName))
cmdTruncationParser =
  OA.optional $
    OA.option
      readTruncation
      ( mconcat
          [ OA.long "cmd-name-trunc",
            OA.short 'x',
            OA.help helpTxt,
            OA.metavar "NATURAL"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of commands/key-names ",
          "in the console logs. Defaults to no truncation. This affects ",
          "everywhere the command/key-name shows up (i.e. in command logs or ",
          "final success/error message); File logs created via --file-log ",
          "are unaffected."
        ]
{-# INLINEABLE cmdTruncationParser #-}

lineTruncationParser :: Parser (Maybe LineTruncation)
lineTruncationParser =
  OA.optional $
    OA.option
      (defRead <|> readDetectTruncation)
      ( mconcat
          [ OA.long "cmd-line-trunc",
            OA.short 'y',
            OA.help helpTxt,
            OA.metavar "<NATURAL | detect>"
          ]
      )
  where
    defRead = Undetected <$> readTruncation
    helpTxt =
      mconcat
        [ "Non-negative integer that limits the length of logs ",
          "produced via --cmd-log in the console logs. Can also be the ",
          "string literal 'detect', to detect the terminal size ",
          "automatically. Defaults to no truncation. This does ",
          "not affect file logs with --file-log."
        ]

readTruncation :: ReadM (Truncation a)
readTruncation = MkTruncation <$> OA.auto
{-# INLINEABLE readTruncation #-}

readDetectTruncation :: ReadM LineTruncation
readDetectTruncation =
  OA.str >>= T.toCaseFold .> \case
    "detect" -> pure Detected
    bad ->
      OA.readerAbort $
        ErrorMsg $
          "Unrecognized truncation option:" <> unpack bad

stripControlParser :: Parser (Maybe StripControl)
stripControlParser =
  OA.optional $
    OA.option
      readStripControl
      ( mconcat
          [ OA.long "strip-control",
            OA.short 's',
            OA.help helpTxt,
            OA.metavar "<all | smart | none>"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Control characters can wreak layout havoc with the --cmd-log",
          " option, thus we include this option. 'all' strips all",
          " such chars. 'none' does nothing i.e. all chars are left",
          " untouched. The default 'smart' attempts to strip",
          " only the control chars that affect layout (e.g. cursor movements) and",
          " leaves others unaffected (e.g. colors). This has the potential",
          " to be the 'prettiest' though it is possible to miss some chars.",
          " This option is experimental and subject to change."
        ]

readStripControl :: ReadM StripControl
readStripControl =
  OA.str >>= T.toCaseFold .> \case
    "all" -> pure StripControlAll
    "none" -> pure StripControlNone
    "smart" -> pure StripControlSmart
    bad ->
      OA.readerAbort $
        ErrorMsg $
          "Unrecognized strip-control option: " <> unpack bad

fileLoggingParser :: Parser (Maybe FilePathDefault)
fileLoggingParser =
  OA.optional $
    OA.option
      readLogFile
      ( mconcat
          [ OA.long "file-log",
            OA.short 'f',
            OA.help helpTxt,
            OA.metavar "[PATH]"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "If a path is supplied, all logs will additionally be written to ",
          "the supplied file. Furthermore, command logs will be written to ",
          "the file irrespective of --cmd-log. Console logging is unaffected. ",
          "This can be useful for investigating command failures. ",
          "If an empty argument is given (i.e. -f '', --file-log=) then we ",
          "write to the Xdg config directory e.g. ",
          "~/.config/shrun/log."
        ]

readLogFile :: ReadM FilePathDefault
readLogFile =
  OA.str >>= \f ->
    if null f
      then pure FPDefault
      else pure (FPManual f)

fileLogModeParser :: Parser (Maybe FileMode)
fileLogModeParser =
  OA.optional $
    OA.option
      readFileMode
      ( mconcat
          [ OA.long "file-log-mode",
            OA.help helpTxt,
            OA.metavar "<append | write>"
          ]
      )
  where
    helpTxt = "Mode in which to open the log file. Defaults to write."

readFileMode :: ReadM FileMode
readFileMode =
  OA.str >>= \case
    "append" -> pure FileModeAppend
    "write" -> pure FileModeWrite
    bad ->
      OA.readerAbort $
        ErrorMsg $
          "Unrecognized --file-log-mode option: " <> unpack bad

fileLogStripControlParser :: Parser (Maybe StripControl)
fileLogStripControlParser =
  OA.optional $
    OA.option
      readStripControl
      ( mconcat
          [ OA.long "file-log-strip-control",
            OA.help helpTxt,
            OA.metavar "<all | smart | none>"
          ]
      )
  where
    helpTxt =
      mconcat
        [ "--strip-control for file logs created with --file-log. ",
          "Defaults to all."
        ]

commandLoggingParser :: Parser (Maybe CmdLogging)
commandLoggingParser =
  OA.optional $
    OA.flag'
      Enabled
      ( mconcat
          [ OA.short 'l',
            OA.long "cmd-log",
            OA.help helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "The default behavior is to swallow logs for the commands ",
          "themselves. This flag gives each command a console region in ",
          "which its logs will be printed. Only the latest log per region ",
          "is show at a given time."
        ]

commandDisplayParser :: Parser (Maybe CmdDisplay)
commandDisplayParser =
  OA.optional $
    OA.flag'
      HideKey
      ( mconcat
          [ OA.short 'k',
            OA.long "key-hide",
            OA.help helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "By default, we display the key name from the legend over the ",
          "actual command that was run, if the former exists. This flag ",
          "instead shows the literal command. Commands without keys are ",
          "unaffected."
        ]

commandsParser :: Parser (NonEmptySeq Text)
commandsParser =
  NESeq.fromNonEmpty
    <$> NE.some1
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )

disableLoggingParser :: Parser (Maybe Bool)
disableLoggingParser =
  OA.optional $
    OA.flag'
      True
      ( mconcat
          [ OA.short 'd',
            OA.long "disable-log",
            OA.help helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "The option disables _all_ logging. This is primarily useful for ",
          "debugging or testing where logging is undesirable."
        ]
