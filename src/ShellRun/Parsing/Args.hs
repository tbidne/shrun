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
import Numeric.Algebra (ASemigroup (..), MSemigroup (..))
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Refined (NonNegative, Refined)
import Refined qualified as R
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Prelude
import Text.Read qualified as Read
import Text.Regex.PCRE ((=~))

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
                ( "Non-negative integer setting a timeout."
                    <> "Can either be a raw number (interpreted as seconds)"
                    <> ", or a \"time string\", e.g., 1d2h3m4s, 2h3s."
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

-- Parses e.g. 1d2h3m4s
regex :: String
regex = "^(?:([0-9])+d)?(?:([0-9])+h)?(?:([0-9])+m)?(?:([0-9])+s)?$"

readTimeStr :: ReadM Timeout
readTimeStr = do
  v :: String <- OApp.str
  let (_, _, _, matches) = v =~ regex :: (String, String, String, [String])
  case matches of
    ms@[d, h, m, s] ->
      let zero = $$(R.refineTH @NonNegative @Int 0)
          multipliers :: [Refined NonNegative Int]
          multipliers =
            [ $$(R.refineTH 86_400),
              $$(R.refineTH 3_600),
              $$(R.refineTH 60),
              $$(R.refineTH 1)
            ]
          txtMultipliers = zip (fmap T.pack ms) multipliers
          results = traverse parseTextAndMultiply txtMultipliers
          summed = foldl' (.+.) zero <$> results
       in case summed of
            Left err -> OApp.readerAbort err
            Right nn -> pure $ MkTimeout nn
    _ ->
      OApp.readerAbort $
        ErrorMsg
          "Could not parse text as time string. Wanted e.g. 1d2h3m4s"

parseTextAndMultiply :: (Text, Refined NonNegative Int) -> Either ParseError (Refined NonNegative Int)
parseTextAndMultiply ("", _) = Right $$(R.refineTH @NonNegative @Int 0)
parseTextAndMultiply (txt, multiplier) =
  let result = textToNonNegative txt
   in fmap (.*. multiplier) result

textToNonNegative :: Text -> Either ParseError (Refined NonNegative Int)
textToNonNegative = textToInt >=> intToNN
  where
    textToInt txt = case Read.readMaybe unpacked of
      Nothing -> Left $ ErrorMsg $ "Could not parse <" <> unpacked <> "> as number"
      Just n -> Right n
      where
        unpacked = T.unpack txt
    intToNN n = first (const err) $ R.refine n
      where
        err = ErrorMsg $ "Wanted non-negative, found: " <> show n

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
