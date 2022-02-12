-- | Provides parsing a legend file into a 'LegendMap'.
module ShellRun.Parsing.Legend
  ( legendPathToMap,
  )
where

import Control.Exception qualified as Ex
import Data.Text qualified as T
import ShellRun.Data.Legend (LegendErr (..), LegendMap)
import ShellRun.Parsing.Legend.Internal qualified as Internal
import ShellRun.Prelude

-- | Given a filepath, attempts to parse the file's contents into
-- a 'LegendMap'. If the file does not exist or the parsing fails
-- (see 'Internal.linesToMap'), an error will be returned.
legendPathToMap :: FilePath -> IO (Either LegendErr LegendMap)
legendPathToMap legendPath = do
  res <- Ex.try (readFileUtf8Lenient legendPath) :: IO (Either Ex.SomeException Text)
  pure $ case res of
    Left err ->
      Left $
        FileErr $
          "Error reading legend file `"
            <> T.pack legendPath
            <> "`: "
            <> showt err
    Right contents -> Internal.linesToMap $ T.lines contents
