{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Parses command line  into the core 'Env' type used by the main
-- application.
module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Control.Applicative ((<**>))
import Control.Applicative qualified as App
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Monad.STM qualified as STM
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Logging (Log, LogQueue (..))
import ShellRun.Math (NonNegative)
import ShellRun.Math qualified as Math
import ShellRun.Types.Env (CommandDisplay (..), Env (..), SubLogging (..))

data Args = MkArgs
  { aLegend :: Maybe Text,
    aTimeout :: Maybe NonNegative,
    aSubLogging :: SubLogging,
    aCommandDisplay :: CommandDisplay,
    aCommands :: [Text]
  }

-- | Runs the parser.
runParser :: IO Env
runParser = do
  queue <- STM.atomically $ TBQueue.newTBQueue 1000
  args <- OptApp.execParser parserInfo
  pure $ toEnv queue args

toEnv :: TBQueue Log -> Args -> Env
toEnv queue MkArgs {aLegend, aTimeout, aSubLogging, aCommandDisplay, aCommands} =
  MkEnv aLegend aTimeout aSubLogging aCommandDisplay (MkLogQueue queue) aCommands

parserInfo :: ParserInfo Args
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

envParser :: Parser Args
envParser =
  MkArgs
    <$> legendParser
    <*> timeoutParser
    <*> subLoggingParser
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

subLoggingParser :: Parser SubLogging
subLoggingParser =
  OptApp.flag
    Disabled
    Enabled
    ( OptApp.short 's'
        <> OptApp.long "sub-logging"
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
