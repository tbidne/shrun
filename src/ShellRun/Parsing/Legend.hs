{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Legend
  ( legendPathToMap,
  )
where

import Control.Exception qualified as Ex
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Legend (LegendErr (..), LegendMap)
import ShellRun.Parsing.Legend.Internal qualified as Internal

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
    Right contents -> Internal.linesToMap $ T.lines $ T.pack contents
  where
    strPath = T.unpack legendPath