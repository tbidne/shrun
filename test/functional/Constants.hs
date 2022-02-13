-- | Provides useful constants.
module Constants
  ( -- * Logging messages
    cancelled,

    -- * Logging color prefixes
    subCommandPrefix,
    infoSuccessPrefix,
    totalTime,
    errPrefix,

    -- * Miscellaneous
    legendPath,
  )
where

import ShellRun.Prelude
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = "Timed out, cancelling remaining tasks."

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = "Finished! Total time elapsed: "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text
subCommandPrefix txt = "[Command] " <> txt

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix txt = "[Info] Successfully ran `" <> txt <> "`. Time elapsed:"

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix txt = "[Error] Error running `" <> txt <> "`:"

-- | Get test legend path
legendPath :: IO [Char]
legendPath = (</> lp) <$> Dir.getCurrentDirectory
  where
    lp = "test/functional/functional_legend.txt"
