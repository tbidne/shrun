{-# LANGUAGE ImportQualifiedPost #-}

-- | Provides functionality for parsing command line arguments.
module ShellRun.Parsing.Args
  ( Args (..),
    parserInfoArgs,
  )
where

import Control.Applicative ((<**>), (<|>))
import Control.Applicative qualified as App
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Data.Env (CommandDisplay (..), CommandLogging (..))
import ShellRun.Math (NonNegative)
import ShellRun.Math qualified as Math

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
  (MkArgs l t cl cd c) <> (MkArgs l' t' cl' cd' c') =
    MkArgs (l <> l') (t <|> t') (cl <> cl') (cd <> cd') (c <> c')

instance Monoid Args where
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
          readNN
          ( OptApp.long "timeout"
              <> OptApp.short 't'
              <> OptApp.help "Non-negative integer setting a timeout."
          )
   in App.optional intParser
  where
    readNN = do
      v <- OptApp.auto
      case Math.mkNonNegative v of
        Just n -> pure n
        Nothing ->
          OptApp.readerAbort $
            ErrorMsg $
              "Timeout must be non-negative, received: "
                <> show v
                <> "!"

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
