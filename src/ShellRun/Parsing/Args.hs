{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for parsing command line arguments.
module ShellRun.Parsing.Args
  ( Args (..),
    parserInfoArgs,
  )
where

import Control.Applicative qualified as App
import Data.List qualified as L
import Data.String (IsString (..), String)
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Refined (NonNegative)
import Refined qualified as R
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Data.TH qualified as TH
import ShellRun.Data.TimeRep (TimeRep (..))
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Prelude
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | Type for parsing command line args.
data Args = MkArgs
  { aCommandLogging :: CommandLogging,
    aFileLogging :: Maybe FilePath,
    aCommandDisplay :: CommandDisplay,
    aLegend :: Maybe FilePath,
    aTimeout :: Maybe Timeout,
    aCommands :: List Text
  }
  deriving (Eq, Show)

instance Semigroup Args where
  (<>) :: Args -> Args -> Args
  (MkArgs cl fp cd l t c) <> (MkArgs cl' fp' cd' l' t' c') =
    MkArgs (cl <> cl') (fp <|> fp') (cd <> cd') (l <> l') (t <|> t') (c <> c')

instance Monoid Args where
  mempty :: Args
  mempty = MkArgs mempty empty mempty empty empty mempty

-- | 'ParserInfo' type for parsing 'Args'.
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
    header = Just "Shell-Run: A tool for running shell commands ergonomically"
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
    <*> fileLoggingParser
    <*> commandDisplayParser
    <*> legendParser
    <*> timeoutParser
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

legendParser :: Parser (Maybe FilePath)
legendParser =
  App.optional
    ( OApp.strOption
        ( OApp.long "legend"
            <> OApp.short 'l'
            <> OApp.help legendHelp
            <> OApp.metavar "PATH"
        )
    )
  where
    legendHelp =
      "Path to legend file, used for translating commands."
        <> " Key/value pairs have the form `key=cmd1,,cmd2,,...`"
        <> ", i.e., keys can refer to multiple commands and refer to"
        <> " other keys recursively. Lines starting with `#` are"
        <> " considered comments and ignored."

timeoutParser :: Parser (Maybe Timeout)
timeoutParser =
  let intParser =
        OApp.option
          (readTimeSeconds <|> readTimeStr)
          ( OApp.long "timeout"
              <> OApp.short 't'
              <> OApp.help
                ( "Non-negative integer setting a timeout. "
                    <> "Can either be a raw number (interpreted as seconds)"
                    <> ", or a \"time string\" e.g. 1d2h3m4s, 2h3s."
                )
              <> OApp.metavar "VAL"
          )
   in App.optional intParser

readTimeSeconds :: ReadM Timeout
readTimeSeconds = do
  v <- OApp.auto
  case R.refine v of
    Right n -> pure $ MkTimeout n
    Left _ ->
      OApp.readerAbort $
        ErrorMsg $
          "Timeout must be non-negative, received: "
            <> show v
            <> "!"

readTimeStr :: ReadM Timeout
readTimeStr = do
  v :: String <- OApp.str
  case MP.parse parseTimeRep "ShellRun.Parsing.Args" v of
    Left _ ->
      OApp.readerAbort $
        ErrorMsg $
          "Wanted time string e.g. 1d2h3m4s. Received: " <> v
    Right timeRep ->
      let timeout = MkTimeout $ TimeRep.toSeconds timeRep
       in pure timeout

fileLoggingParser :: Parser (Maybe FilePath)
fileLoggingParser =
  App.optional
    ( OApp.strOption
        ( OApp.long "file-logging"
            <> OApp.short 'f'
            <> OApp.help help
            <> OApp.metavar "PATH"
        )
    )
  where
    help =
      "If a path is supplied, all logs will additionally be written to the "
        <> "supplied file. Normal logging (i.e. stdout) is unaffected. This "
        <> "can be useful for investigating subcommand failure."

commandLoggingParser :: Parser CommandLogging
commandLoggingParser =
  OApp.flag
    Disabled
    Enabled
    ( OApp.short 'c'
        <> OApp.long "command-logging"
        <> OApp.help "Adds Commands' logs (stdout+stderr) to output."
    )

commandDisplayParser :: Parser CommandDisplay
commandDisplayParser =
  OApp.flag
    ShowCommand
    ShowKey
    ( OApp.short 'k'
        <> OApp.long "key-show"
        <> OApp.help
          ( "In output, display key name over actual command if it"
              <> " exists."
          )
    )

commandsParser :: Parser (List Text)
commandsParser =
  App.some
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

parseTimeOrZero :: Char -> MParser RNonNegative
parseTimeOrZero c =
  -- Backtrack if we don't match
  MP.try (parseNNWithUnit c)
    <|> pure TH.zeroNN

parseNNWithUnit :: Char -> MParser RNonNegative
parseNNWithUnit c = parseNonNegative <* MPC.char' c

parseNonNegative :: MParser RNonNegative
parseNonNegative = do
  ds <- MP.some MPC.digitChar
  case TR.readMaybe ds of
    Nothing -> MP.customFailure $ "Could not parse natural: " <> showt ds
    Just n -> case R.refine @NonNegative @Int n of
      Left ex -> MP.customFailure $ "Refinment failed: " <> showt ex
      Right n' -> pure n'
