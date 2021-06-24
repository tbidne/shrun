{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides useful constants.
module Constants
  ( -- * Logging messages
    cancelled,

    -- * Logging color prefixes
    subCommandPrefix,
    infoSuccessPrefix,
    infoBluePrefix,
    totalTime,
    warnPrefix,
    errPrefix,

    -- * Miscellaneous
    workingDirectory,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import ShellRun.Logging (LogLevel (..), levelToColor, levelToPrefix)
import System.Console.Pretty qualified as P

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = warnPrefix "Timed out, cancelling remaining tasks."

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text -> Text
totalTime = infoBluePrefix . (<>) "Finished! Total time elapsed: "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text
subCommandPrefix = withFormatting SubCommand

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix = withFormatting InfoSuccess

-- | Expected blue info 'Text'.
infoBluePrefix :: Text -> Text
infoBluePrefix = withFormatting InfoBlue

-- | Expected warning 'Text'.
warnPrefix :: Text -> Text
warnPrefix = withFormatting Warn

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix = withFormatting Error

withFormatting :: LogLevel -> Text -> Text
withFormatting lvl = P.color color . (<>) prefix
  where
    color = levelToColor lvl
    prefix = levelToPrefix lvl

-- | Functional test working directory.
workingDirectory :: IsString a => a
workingDirectory = "./test/functional/scripts"
