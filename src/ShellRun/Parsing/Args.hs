-- | Provides functionality for parsing command line arguments.
module ShellRun.Parsing.Args
  ( Args (..),
    parserInfoArgs,
  )
where

import Control.Applicative qualified as App
import Data.String (String)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Math (NonNegative, Positive, (*:*), (+:+))
import ShellRun.Math qualified as Math
import ShellRun.Prelude
import Text.Read qualified as Read
import Text.Regex.PCRE ((=~))

-- | Type for parsing command line args.
data Args = MkArgs
  { aLegend :: Maybe Text,
    aTimeout :: Maybe NonNegative,
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
      <**> OptApp.helper

legendParser :: Parser (Maybe Text)
legendParser =
  App.optional
    ( T.pack
        <$> OptApp.strOption
          ( OptApp.long "legend"
              <> OptApp.short 'l'
              <> OptApp.help legendHelp
              <> OptApp.metavar "PATH"
          )
    )
  where
    legendHelp =
      "Path to legend file, used for translating commands."
        <> " Key/value pairs have the form `key=cmd1,,cmd2,,...`"
        <> ", i.e., keys can refer to multiple commands and refer to"
        <> " other keys recursively. Lines starting with `#` are"
        <> " considered comments and ignored."

timeoutParser :: Parser (Maybe NonNegative)
timeoutParser =
  let intParser =
        OptApp.option
          (readTimeSeconds <|> readTimeStr)
          ( OptApp.long "timeout"
              <> OptApp.short 't'
              <> OptApp.help
                ( "Non-negative integer setting a timeout."
                    <> "Can either be a raw number (interpreted as seconds)"
                    <> ", or a \"time string\", e.g., 1d2h3m4s, 2h3s."
                )
              <> OptApp.metavar "VAL"
          )
   in App.optional intParser

readTimeSeconds :: ReadM NonNegative
readTimeSeconds = do
  v <- OptApp.auto
  case Math.mkNonNegative v of
    Just n -> pure n
    Nothing ->
      OptApp.readerAbort $
        ErrorMsg $
          "Timeout must be non-negative, received: "
            <> show v
            <> "!"

-- Parses e.g. 1d2h3m4s
regex :: String
regex = "^(?:([0-9])+d)?(?:([0-9])+h)?(?:([0-9])+m)?(?:([0-9])+s)?$"

readTimeStr :: ReadM NonNegative
readTimeStr = do
  v :: String <- OptApp.str
  let (_, _, _, matches) = v =~ regex :: (String, String, String, [String])
  case matches of
    [d, h, m, s] ->
      let txtMultipliers =
            bimap T.pack Math.unsafePositive
              <$> [ (d, 86_400),
                    (h, 3_600),
                    (m, 60),
                    (s, 1)
                  ]
          results = traverse parseTextAndMultiply txtMultipliers
          summed = foldl' (+:+) (Math.unsafeNonNegative 0) <$> results
       in case summed of
            Left err -> OptApp.readerAbort err
            Right nn -> pure nn
    _ ->
      OptApp.readerAbort $
        ErrorMsg
          "Could not parse text as time string. Wanted e.g. 1d2h3m4s"

parseTextAndMultiply :: (Text, Positive) -> Either ParseError NonNegative
parseTextAndMultiply ("", _) = Right $ Math.unsafeNonNegative 0
parseTextAndMultiply (txt, multiplier) =
  let result = textToNonNegative txt
   in fmap (*:* multiplier) result

textToNonNegative :: Text -> Either ParseError NonNegative
textToNonNegative = textToInt >=> intToNN
  where
    textToInt txt = case Read.readMaybe unpacked of
      Nothing -> Left $ ErrorMsg $ "Could not parse <" <> unpacked <> "> as number"
      Just n -> Right n
      where
        unpacked = T.unpack txt
    intToNN n = maybeToEither err $ Math.mkNonNegative n
      where
        err = ErrorMsg $ "Wanted non-negative, found: " <> show n

commandLoggingParser :: Parser CommandLogging
commandLoggingParser =
  OptApp.flag
    Disabled
    Enabled
    ( OptApp.short 'c'
        <> OptApp.long "command-logging"
        <> OptApp.help "Adds Commands' logs (stdout+stderr) to output."
    )

commandDisplayParser :: Parser CommandDisplay
commandDisplayParser =
  OptApp.flag
    ShowCommand
    ShowKey
    ( OptApp.short 'k'
        <> OptApp.long "show-key"
        <> OptApp.help
          ( "In output, display key name over actual command if it"
              <> " exists."
          )
    )

commandsParser :: Parser [Text]
commandsParser =
  App.some
    ( T.pack
        <$> OptApp.argument OptApp.str (OptApp.metavar "Commands...")
    )
