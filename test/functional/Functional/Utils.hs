-- | Provides utilities.
module Functional.Utils
  ( -- * Logging messages
    cancelled,
    totalTime,

    -- * Logging color prefixes
    subCommandPrefix,
    infoSuccessPrefix,
    errPrefix,
  )
where

import Functional.Prelude

-- | Expected timeout 'Text'.
cancelled :: Text
cancelled = "Timed out, cancelling remaining commands:"

-- | Expected total \"Time elapsed\"" 'Text'.
totalTime :: Text
totalTime = "Finished! Total time elapsed: "

-- | Expected success 'Text'.
subCommandPrefix :: Text -> Text -> Text
subCommandPrefix cmd txt = "[Command] [" <> cmd <> "] " <> txt

-- | Expected success 'Text'.
infoSuccessPrefix :: Text -> Text
infoSuccessPrefix txt = "[Info] [" <> txt <> "] Success. Time elapsed:"

-- | Expected error 'Text'.
errPrefix :: Text -> Text
errPrefix txt = "[Error] [" <> txt <> "] Error:"