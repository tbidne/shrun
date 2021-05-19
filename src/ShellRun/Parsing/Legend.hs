{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Legend
  ( legendPathToMap,
  )
where

import Control.Applicative qualified as App
import Control.Exception qualified as Ex
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

legendPathToMap :: Text -> IO (Either LegendErr LegendMap)
legendPathToMap legendPath = do
  res <- Ex.try (readFile strPath) :: IO (Either Ex.SomeException String)
  pure $ case res of
    Left err ->
      Left $
        FileErr $
          "Error reading legend file `"
            <> legendPath
            <> "`: "
            <> T.pack (show err)
    Right contents -> linesToMap $ T.lines $ T.pack contents
  where
    strPath = T.unpack legendPath

linesToMap :: [Text] -> Either LegendErr LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = App.liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) = Map.insert key cmd

parseLine :: Text -> Either LegendErr (Text, Text)
parseLine l =
  case T.splitOn "=" l of
    ["", _] -> Left $ ParseErr l
    [_, ""] -> Left $ ParseErr l
    [key, cmd] -> Right (key, cmd)
    _ -> Left $ ParseErr l