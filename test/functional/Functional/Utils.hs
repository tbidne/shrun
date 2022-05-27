-- | Provides utilities.
module Functional.Utils
  ( -- * Logging messages
    cancelled,
    totalTime,

    -- * Logging color prefixes
    subCommandPrefix,
    infoSuccessPrefix,
    errPrefix,

    -- * IO
    runAndGetLogs,
  )
where

import Functional.FuncEnv qualified as FuncEnv
import Functional.Prelude
import ShellRun qualified as SR
import System.Environment qualified as SysEnv

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

runAndGetLogs :: List (List Char) -> IO (IORef [Text])
runAndGetLogs argList = do
  funcEnv <- SysEnv.withArgs argList FuncEnv.mkFuncEnv
  SR.runShellT SR.runShell funcEnv
  pure $ funcEnv ^. #logs
