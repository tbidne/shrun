{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for parsing command line arguments.
module ShellRun.Parsing.Args
  ( Args (..),
    parserInfoArgs,
  )
where

import Control.Applicative qualified as App
import Data.List qualified as L
import Data.String (String)
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Refined (NonNegative, Refined)
import Refined qualified as R
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
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
  { aLegend :: Maybe Text,
    aTimeout :: Maybe Timeout,
    aCommandLogging :: CommandLogging,
    aCommandDisplay :: CommandDisplay,
    aCommands :: [Text]
  }
  deriving (Eq, Show)

instance Semigroup Args where
  (<>) :: Args -> Args -> Args
  (MkArgs l t cl cd c) <> (MkArgs l' t' cl' cd' c') =
    MkArgs (l <> l') (t <|> t') (cl <> cl') (cd <> cd') (c <> c')

instance Monoid Args where
  mempty :: Args
  mempty = MkArgs mempty Nothing mempty mempty mempty

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> legendParser
    <*> timeoutParser
    <*> commandLoggingParser
    <*> commandDisplayParser
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
          "Version: " <> $$(PV.packageVersionStringTH "shell-run.cabal"),
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

legendParser :: Parser (Maybe Text)
legendParser =
  App.optional
    ( T.pack
        <$> OApp.strOption
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
        <> OApp.long "show-key"
        <> OApp.help
          ( "In output, display key name over actual command if it"
              <> " exists."
          )
    )

commandsParser :: Parser [Text]
commandsParser =
  App.some
    ( T.pack
        <$> OApp.argument OApp.str (OApp.metavar "Commands...")
    )

type MParser = Parsec Text [Char]

parseTimeRep :: MParser TimeRep
parseTimeRep =
  MkTimeRep
    <$> parseTimeOrZero 'd'
    <*> parseTimeOrZero 'h'
    <*> parseTimeOrZero 'm'
    <*> parseTimeOrZero 's'
    <* MP.eof

parseTimeOrZero :: Char -> MParser (Refined NonNegative Int)
parseTimeOrZero c =
  -- Backtrack if we don't match
  MP.try (parseNNWithUnit c)
    <|> pure zero

parseNNWithUnit :: Char -> MParser (Refined NonNegative Int)
parseNNWithUnit c = parseNonNegative <* MPC.char' c

parseNonNegative :: MParser (Refined NonNegative Int)
parseNonNegative = do
  ds <- MP.some MPC.digitChar
  case TR.readMaybe ds of
    Nothing -> MP.customFailure $ "Could not parse natural: " <> showt ds
    Just n -> case R.refine @NonNegative @Int n of
      Left ex -> MP.customFailure $ "Refinment failed: " <> showt ex
      Right n' -> pure n'

zero :: Refined NonNegative Int
zero = $$(R.refineTH 0)
