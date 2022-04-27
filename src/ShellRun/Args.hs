{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command line arguments.
--
-- @since 0.1
module ShellRun.Args
  ( Args (..),
    ALineTruncation (..),
    defaultArgs,
    parserInfoArgs,
  )
where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..), String)
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Data.FilePathDefault (FilePathDefault (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.TimeRep (TimeRep (..))
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env.Types
  ( CmdDisplay (..),
    CmdLogging (..),
    TruncRegion (..),
    Truncation (..),
  )
import ShellRun.Prelude
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- $setup
-- >>> :{
--   prettyArgs :: Args -> Text
--   prettyArgs args =
--    "MkArgs"
--      <> "\n  { cmdLogging = " <> showt (args ^. #cmdLogging)
--      <> ",\n    fileLogging = " <> showt (args ^. #fileLogging)
--      <> ",\n    cmdDisplay = " <> showt (args ^. #cmdDisplay)
--      <> ",\n    legend = " <> showt (args ^. #legend)
--      <> ",\n    timeout = " <> showt (args ^. #timeout)
--      <> ",\n    cmdNameTrunc = " <> showt (args ^. #cmdNameTrunc)
--      <> ",\n    cmdLineTrunc = " <> showt (args ^. #cmdLineTrunc)
--      <> ",\n    globalLogging = " <> showt (args ^. #globalLogging)
--      <> ",\n    commands = " <> showt (args ^. #commands)
--      <> "\n  }"
-- :}

-- | Determines command log line truncation behavior. We need a separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
--
-- @since 0.1
data ALineTruncation
  = -- | @since 0.1
    Undetected (Truncation 'TCmdLine)
  | -- | @since 0.1
    Detected
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrismLabels ''ALineTruncation

-- | Type for parsing command line args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Whether to log commands.
    --
    -- @since 0.1
    cmdLogging :: CmdLogging,
    -- | Overarching option for logging. If it is false then all logging is
    -- disabled.
    --
    -- @since 0.1
    globalLogging :: Bool,
    -- | Optional path to log file.
    --
    -- @since 0.1
    fileLogging :: FilePathDefault,
    -- | Whether to display command by (key) name or command.
    --
    -- @since 0.1
    cmdDisplay :: CmdDisplay,
    -- | Optional legend file.
    --
    -- @since 0.1
    legend :: FilePathDefault,
    -- | Timeout.
    --
    -- @since 0.1
    timeout :: Timeout,
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1
    cmdNameTrunc :: Truncation 'TCmdName,
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1
    cmdLineTrunc :: ALineTruncation,
    -- | List of commands.
    --
    -- @since 0.1
    commands :: NonEmptySeq Text
  }
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makeFieldLabelsNoPrefix ''Args

-- | @since 0.1
instance Semigroup ALineTruncation where
  Undetected x <> Undetected y = Undetected (x <> y)
  _ <> _ = Detected

-- | @since 0.1
instance Monoid ALineTruncation where
  mempty = Undetected mempty

-- | Default configuration.
--
-- ==== __Examples__
-- >>> putStrLn $ prettyArgs $ defaultArgs (NESeq.singleton "ls")
-- MkArgs
--   { cmdLogging = Disabled,
--     fileLogging = FPNone,
--     cmdDisplay = ShowCmd,
--     legend = FPDefault,
--     timeout = MkTimeout {unTimeout = PPosInf},
--     cmdNameTrunc = MkTruncation {unTruncation = PPosInf},
--     cmdLineTrunc = Undetected (MkTruncation {unTruncation = PPosInf}),
--     globalLogging = True,
--     commands = "ls" :|^ fromList []
--   }
--
-- @since 0.1
defaultArgs :: NonEmptySeq Text -> Args
defaultArgs cmds =
  MkArgs
    { cmdLogging = mempty,
      fileLogging = mempty,
      cmdDisplay = mempty,
      legend = FPDefault,
      timeout = mempty,
      cmdNameTrunc = mempty,
      cmdLineTrunc = mempty,
      globalLogging = True,
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
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Shell-Run: A tool for running shell commands concurrently."
    footer = Just $ fromString versNum
    desc =
      Just $
        "\nShell-Run runs shell commands concurrently. In addition to "
          <> "providing basic timing and logging functionality, we also provide "
          <> "the ability to pass in a legend file that can be used to define "
          <> "aliases for commands. See github.com/tbidne/shell-run#README for "
          <> "full documentation."

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> commandLoggingParser
    <*> globalLoggingParser
    <*> fileLoggingParser
    <*> commandDisplayParser
    <*> legendParser
    <*> timeoutParser
    <*> cmdTruncationParser
    <*> lineTruncationParser
    <*> commandsParser
      <**> OApp.helper
      <**> version

version :: Parser (a -> a)
version = OApp.infoOption txt (OApp.long "version" <> OApp.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Shell-Run",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: List Char
versNum = "Version: " <> $$(PV.packageVersionStringTH "shell-run.cabal")

legendParser :: Parser FilePathDefault
legendParser =
  OApp.option
    readLogFile
    ( OApp.value FPDefault
        <> OApp.long "legend"
        <> OApp.short 'l'
        <> OApp.help legendHelp
        <> OApp.metavar "PATH"
    )
  where
    legendHelp =
      "Path to legend file, used for translating commands. "
        <> "Key/value pairs have the form 'key=cmd1,,cmd2,,...' "
        <> ", i.e., keys can refer to multiple commands and refer to "
        <> "other keys recursively. Lines starting with '#' are "
        <> "considered comments and ignored. If no path is given, we "
        <> "automatically look in the Xdg config directory e.g. "
        <> "~/.config/shell-run/shell-run.legend."

timeoutParser :: Parser Timeout
timeoutParser =
  OApp.option
    (readTimeSeconds <|> readTimeStr)
    ( OApp.value (MkTimeout PPosInf)
        <> OApp.long "timeout"
        <> OApp.short 't'
        <> OApp.help help
        <> OApp.metavar "NATURAL"
    )
  where
    help =
      "Non-negative integer setting a timeout. Can either be a raw number "
        <> "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or "
        <> "2h3s. Defaults to no timeout."

readTimeSeconds :: ReadM Timeout
readTimeSeconds = MkTimeout . PFin <$> OApp.auto

readTimeStr :: ReadM Timeout
readTimeStr = do
  v :: String <- OApp.str
  case MP.parse parseTimeRep "ShellRun.Args" v of
    Left _ ->
      OApp.readerAbort $
        ErrorMsg $
          "Wanted time string e.g. 1d2h3m4s. Received: " <> v
    Right timeRep ->
      let timeout = MkTimeout $ PFin $ TimeRep.toSeconds timeRep
       in pure timeout

cmdTruncationParser :: Parser (Truncation 'TCmdName)
cmdTruncationParser =
  OApp.option
    readTruncation
    ( OApp.value (MkTruncation PPosInf)
        <> OApp.long "cmd-name-trunc"
        <> OApp.short 'x'
        <> OApp.help help
        <> OApp.metavar "NATURAL"
    )
  where
    help =
      "Non-negative integer that limits the length of commands/key-names "
        <> "in the console logs. Defaults to no truncation. This affects "
        <> "everywhere the command/key-name shows up (i.e. in command logs or "
        <> "final success/error message); File logs created via --file-log "
        <> "are unaffected."

lineTruncationParser :: Parser ALineTruncation
lineTruncationParser =
  OApp.option
    (defRead <|> readDetectTruncation)
    ( OApp.value defValue
        <> OApp.long "cmd-line-trunc"
        <> OApp.short 'y'
        <> OApp.help help
        <> OApp.metavar "NATURAL or detect"
    )
  where
    defValue = Undetected (MkTruncation PPosInf)
    defRead = Undetected <$> readTruncation
    help =
      "Non-negative integer that limits the length of logs "
        <> "produced via --cmd-log in the console logs. Can also be the "
        <> "string literal 'detect' or 'd' (no quotes), to detect the "
        <> "terminal size automatically. Defaults to no truncation. This does "
        <> "not affect file logs with --file-log."

readTruncation :: ReadM (Truncation a)
readTruncation = MkTruncation . PFin <$> OApp.auto

readDetectTruncation :: ReadM ALineTruncation
readDetectTruncation = do
  s <- OApp.str
  let s' = T.toCaseFold s
  if s' == "d" || s' == "detect"
    then pure Detected
    else
      OApp.readerAbort $
        ErrorMsg $ "Unrecognized truncation option:" <> T.unpack s

fileLoggingParser :: Parser FilePathDefault
fileLoggingParser =
  OApp.option
    readLogFile
    ( OApp.value FPNone
        <> OApp.long "file-log"
        <> OApp.short 'f'
        <> OApp.help help
        <> OApp.metavar "PATH"
    )
  where
    help =
      "If a path is supplied, all logs will additionally be written to the "
        <> "supplied file. Furthermore, command logs will be written to the "
        <> "file irrespective of --cmd-log. Console logging is "
        <> "unaffected. This can be useful for investigating command "
        <> "failures. If the string literal 'default' or 'd' is given, we "
        <> "will write to the Xdg config directory e.g. "
        <> "~/.config/shell-run/shell-run.log"

readLogFile :: ReadM FilePathDefault
readLogFile = do
  f <- OApp.str
  if f == "d" || f == "default"
    then pure FPDefault
    else pure (FPManual f)

commandLoggingParser :: Parser CmdLogging
commandLoggingParser =
  OApp.flag
    Disabled
    Enabled
    ( OApp.short 'c'
        <> OApp.long "cmd-log"
        <> OApp.help help
    )
  where
    help =
      "The default behavior is to swallow logs for the commands "
        <> "themselves. This flag gives each command a console region in "
        <> "which its logs will be printed. Only the latest log per region "
        <> "is show at a given time."

commandDisplayParser :: Parser CmdDisplay
commandDisplayParser =
  OApp.flag
    ShowCmd
    ShowKey
    ( OApp.short 'k'
        <> OApp.long "key-show"
        <> OApp.help help
    )
  where
    help =
      "In console output, display key name from legend file over actual "
        <> "command if it exists."

commandsParser :: Parser (NonEmptySeq Text)
commandsParser =
  NESeq.unsafeFromNonEmpty
    <$> NE.some1
      ( T.pack
          <$> OApp.argument OApp.str (OApp.metavar "Commands...")
      )

type MParser = Parsec Text (List Char)

parseTimeRep :: MParser TimeRep
parseTimeRep =
  MkTimeRep
    <$> parseTimeOrZero 'd'
    <*> parseTimeOrZero 'h'
    <*> parseTimeOrZero 'm'
    <*> parseTimeOrZero 's'
    <* MP.eof

parseTimeOrZero :: Char -> MParser Natural
parseTimeOrZero c =
  -- Backtrack if we don't match
  MP.try (parseNNWithUnit c)
    <|> pure 0

parseNNWithUnit :: Char -> MParser Natural
parseNNWithUnit c = parseNonNegative <* MPC.char' c

parseNonNegative :: MParser Natural
parseNonNegative = do
  ds <- MP.some MPC.digitChar
  case TR.readMaybe ds of
    Nothing -> MP.customFailure $ "Could not parse natural: " <> showt ds
    Just n -> pure n

globalLoggingParser :: Parser Bool
globalLoggingParser =
  OApp.flag
    True
    False
    ( OApp.short 'd'
        <> OApp.long "disable-log"
        <> OApp.help help
    )
  where
    help =
      "The option disables _all_ logging. This is primarily useful for "
        <> "debugging or testing where logging is undesirable."
