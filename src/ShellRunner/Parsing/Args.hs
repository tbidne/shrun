module ShellRunner.Parsing.Args
  ( Args (..),
    runParser,
  )
where

import Control.Applicative qualified as App
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRunner.Types (NonNegative)
import ShellRunner.Types.NonNegative qualified as NN

data Args = MkArgs
  { legend :: Maybe Text,
    timeout :: Maybe NonNegative,
    commands :: [Text]
  }
  deriving (Show)

runParser :: IO Args
runParser = OptApp.execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo =
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
    <*> commandsParser

legendParser :: Parser (Maybe Text)
legendParser =
  App.optional
    ( T.pack
        <$> OptApp.strOption
          ( OptApp.long "legend"
              <> OptApp.short 'l'
              <> OptApp.help "Path to legend file, used for translating commands"
          )
    )

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

commandsParser :: Parser [Text]
commandsParser = App.some (T.pack <$> OptApp.argument OptApp.str (OptApp.metavar "[Commands]"))