{-# LANGUAGE OverloadedStrings #-}

-- | Provides useful constants.
module Constants
  ( -- * Logging messages
    cancelled,
    timeCmd,
    totalTime,

    -- * Logging color prefixes
    infoSuccessPrefix,
    infoBluePrefix,
    warnPrefix,
    errPrefix,

    -- * Miscellaneous
    workingDirectory,
  )
where

import Data.String (IsString)
import Data.Text (Text)

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = warnPrefix <> "Timed out, cancelling remaining tasks"

-- | Expected command \"Time elapsed\"" 'Text'.
timeCmd :: Text
timeCmd = infoSuccessPrefix <> "Time elapsed: "

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = infoBluePrefix <> "Total time elapsed: "

-- | Expected success 'Text'.
infoSuccessPrefix :: Text
infoSuccessPrefix = "\ESC[92m[Info] "

-- | Expected blue info 'Text'.
infoBluePrefix :: Text
infoBluePrefix = "\ESC[94m[Info] "

-- | Expected warning 'Text'.
warnPrefix :: Text
warnPrefix = "\ESC[95m[Warn] "

-- | Expected error 'Text'.
errPrefix :: Text
errPrefix = "\ESC[91m[Error] "

-- | Functional test working directory.
workingDirectory :: IsString a => a
workingDirectory = "./test/functional/scripts"
