{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Control.Applicative ((<**>))
import Control.Applicative qualified as App
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Types.Env (Env (..), NativeLog (..))
import ShellRun.Types.NonNegative (NonNegative)
import ShellRun.Types.NonNegative qualified as NN

runParser :: IO Env
runParser = OptApp.execParser parserInfo

parserInfo :: ParserInfo Env
parserInfo =
  ParserInfo
    { infoParser = envParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

envParser :: Parser Env
envParser =
  MkEnv
    <$> legendParser
    <*> timeoutParser
    <*> nativeLogParser
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
              <> OptApp.help "Non-negative integer setting a timeout"
          )
   in App.optional intParser
  where
    readNN = do
      v <- OptApp.auto
      case NN.mkNonNegative v of
        Just n -> pure n
        Nothing ->
          OptApp.readerAbort $
            ErrorMsg $
              "Timeout must be non-negative, received: "
                <> show v
                <> "!"

nativeLogParser :: Parser NativeLog
nativeLogParser =
  OptApp.flag
    None
    Stdout
    ( OptApp.long "nativeLog"
        <> OptApp.short 'n'
        <> OptApp.help
          ( "If this is flag is on, we will log all commands' stdout. "
              <> "The default behavior is to swallow stdout."
          )
    )

commandsParser :: Parser [Text]
commandsParser =
  App.some
    ( T.pack
        <$> OptApp.argument OptApp.str (OptApp.metavar "Commands...")
    )