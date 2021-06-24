{-# LANGUAGE OverloadedStrings #-}

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
    workingDirectory,
  )
where

import Data.String (IsString)
import Data.Text (Text)

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = "Timed out, cancelling remaining tasks."

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = "Finished! Total time elapsed: "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text
subCommandPrefix txt = "[SubCommand] " <> txt

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix txt = "[Info] Successfully ran `" <> txt <> "`. Time elapsed:"

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix txt = "[Error] Error running `" <> txt <> "`:"

-- | Functional test working directory.
workingDirectory :: IsString a => a
workingDirectory = "./test/functional/scripts"
