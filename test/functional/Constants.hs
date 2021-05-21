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

cancelled :: Text
cancelled = warnPrefix <> "Timed out, cancelling remaining tasks"

timeCmd :: Text
timeCmd = infoSuccessPrefix <> "Time elapsed: "

totalTime :: Text
totalTime = infoBluePrefix <> "Total time elapsed: "

infoSuccessPrefix :: Text
infoSuccessPrefix = "\ESC[92m[Info] "

infoBluePrefix :: Text
infoBluePrefix = "\ESC[94m[Info] "

warnPrefix :: Text
warnPrefix = "\ESC[95m[Warn] "

errPrefix :: Text
errPrefix = "\ESC[91m[Error] "

workingDirectory :: IsString a => a
workingDirectory = "./test/functional/scripts"